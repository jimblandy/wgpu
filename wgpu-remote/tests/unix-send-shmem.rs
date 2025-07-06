/*! Exercise sending shared memory via Unix domain sockets.

This isn't really a test of the `wgpu-remote` crate's
functionality; it's a verification that the system actually
behaves as `wgpu-remote` expects.

If it turns out that all Unix systems behave identically, then
this is a waste of time. But if it turns out that there are bugs,
quirks, or limits, then this serves to detect and document them.

*/

use std::os::fd::{self, AsRawFd, FromRawFd};

use nix::sys::{memfd, mman, socket, wait};
use nix::unistd;

fn main() {
    let (sender_socket, receiver_socket) = socket::socketpair(
        socket::AddressFamily::Unix,
        socket::SockType::Stream,
        None,
        socket::SockFlag::empty(),
    ).unwrap();

    // Start the sender.
    // Safety: we are single-threaded.
    let fork = unsafe { unistd::fork() }.unwrap();
    let unistd::ForkResult::Parent { child: sender_pid } = fork else {
        drop(receiver_socket);
        sender(sender_socket);
        // never returns
    };

    // Start the receiver.
    // Safety: we are single-threaded.
    let fork = unsafe { unistd::fork() }.unwrap();
    let unistd::ForkResult::Parent { child: receiver_pid } = fork else {
        drop(sender_socket);
        receiver(receiver_socket);
        // never returns
    };

    drop(sender_socket);
    drop(receiver_socket);

    let status = wait::waitpid(sender_pid, None).unwrap();
    assert_eq!(status, wait::WaitStatus::Exited(sender_pid, 42));

    let status = wait::waitpid(receiver_pid, None).unwrap();
    assert_eq!(status, wait::WaitStatus::Exited(receiver_pid, 43));
}

const LEN: usize = 10;

fn sender(socket: fd::OwnedFd) -> ! {
    // Create a file descriptor referring to zeroed memory.
    //
    // The `nix` source code suggests `memfd_create` is available on
    // Linux, Android, and FreeBSD.
    let mem_fd = memfd::memfd_create(
        c"unix-send-shmem",
        memfd::MemFdCreateFlag::empty(),
    ).unwrap();

    // Set the file's size.
    unistd::ftruncate(&mem_fd, LEN as _).unwrap();

    // Map the memory file descriptor into our address space.
    //
    // Safety: We're not requesting an address, our offset is aligned,
    // and our flags are reasonable.
    let mapped = unsafe {
        mman::mmap(
            None,
            std::num::NonZeroUsize::new(LEN).unwrap(),
            mman::ProtFlags::PROT_READ | mman::ProtFlags::PROT_WRITE,
            mman::MapFlags::MAP_SHARED,
            &mem_fd,
            0,
        ).unwrap()
    };

    // Write a message to the memory.
    //
    // Safety: the memory is there and zeroed, and `u8` has no
    // requirement alignments.
    let mapped = unsafe { mapped.cast::<[u8; LEN]>().as_mut() };
    mapped.copy_from_slice(b"Greetings!");

    // Send the file descriptor to the other process.
    //
    // The unix(7) man page for Linux says:
    //
    //     At least one byte of real data should be  sent  when  sending  ancillary
    //     data.   On  Linux,  this is required to successfully send ancillary data
    //     over a UNIX domain stream socket.  When sending ancillary  data  over  a
    //     UNIX  domain  datagram  socket, it is not necessary on Linux to send any
    //     accompanying real data.  However, portable applications should also  in‐
    //     clude  at least one byte of real data when sending ancillary data over a
    //     datagram socket.
    socket::sendmsg::<()>(
        socket.as_raw_fd(),
        &[std::io::IoSlice::new(b"X")], // one byte of "real" data
        &[socket::ControlMessage::ScmRights(&[mem_fd.as_raw_fd()])],
        socket::MsgFlags::empty(),
        None, // address
    ).unwrap();

    std::process::exit(42);
}

fn receiver(socket: fd::OwnedFd) -> ! {
    // Now do a recvmsg that accepts one byte of real data.
    let mut data_buf = [0_u8; 1];
    let mut slices = [std::io::IoSliceMut::new(&mut data_buf)];
    let mut fd_buf = nix::cmsg_space!(fd::RawFd);
    let msg = socket::recvmsg::<()>(
        socket.as_raw_fd(),
        &mut slices, // iov (ordinary bytes)
        Some(&mut fd_buf), // cmsg_buffer
        socket::MsgFlags::empty(),
    ).unwrap();

    // Get the memory file descriptor out of the message.
    assert_eq!(msg.bytes, 1);
    let mut cmsgs = msg.cmsgs().unwrap();
    let mut fds;
    match cmsgs.next() {
        Some(socket::ControlMessageOwned::ScmRights(f)) => { fds = f; }
        Some(other) => panic!("Got unexpected control message: {other:#?}"),
        None => panic!("didn't get any control messages"),
    }
    assert_eq!(cmsgs.next(), None);
    let raw_mem_fd = fds.pop().unwrap();
    // Safety: we just got this fd from a control message, so it should be open.
    let mem_fd = unsafe { fd::OwnedFd::from_raw_fd(raw_mem_fd) };
    assert!(fds.is_empty());

    // Check the byte that was transmitted.
    assert_eq!(&slices[0][..1], b"X");

    // Map the memory file descriptor into our address space.
    //
    // Safety: We're not requesting an address, our offset is aligned,
    // and our flags are reasonable.
    let mapped = unsafe {
        mman::mmap(
            None,
            std::num::NonZeroUsize::new(LEN).unwrap(),
            mman::ProtFlags::PROT_READ,
            mman::MapFlags::MAP_SHARED,
            &mem_fd,
            0,
        ).unwrap()
    };

    // Check the message in the memory.
    //
    // Safety: the memory is there and initialized, and `u8` has no
    // requirement alignments.
    let mapped = unsafe { mapped.cast::<[u8; LEN]>().as_ref() };
    assert_eq!(mapped, b"Greetings!");

    std::process::exit(43);
}

[numthreads(1, 1, 1)]
void main()
{
    float3 a = float3(0.0, 0.0, 1.0);
    float2x3 m = float2x3(1, 2, 3, 4, 5, 6); 
    float2 x = mul(m, a);

    float3x2 n = float3x2(9, 8, 7, 6, 5, 4);
    float2x2 y = mul(m, n);

    return;
}

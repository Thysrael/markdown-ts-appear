| Bit | Name | Description |
|----|----|----|
| 0 | x87 state | 控制 x87 浮点单元的状态保存和加载。 |
| 1 | SSE state | 控制 SSE 指令集的状态（XMM 寄存器）保存和加载。 |
| 2 | AVX state | 控制 AVX 指令集的状态（YMM 寄存器的高位 256 位）保存和加载。 |
| 3 | MPX state | 控制 MPX 寄存器状态的保存和加载。 |
| 4 | AVX-512 state | 控制 AVX-512 指令集的状态保存和加载。 |
| 5-7 | AVX-512 state extended components | 控制 AVX-512 指令集更多状态（如 ZMM 寄存器）的保存和加载。 |
| 8 | PKRU state | 控制保护密钥寄存器（用于内存保护扩展）的状态保存和加载。 |
| … | … | 其他保畷位或特定处理器可能定义的特定位。 |

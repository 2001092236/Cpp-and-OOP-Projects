# My-projects
1) Implemented **String** class for working with strings. With depreciated running time O(1) on push_back, pop_back operations and linear memory usage.
2) The **BigInteger and Rational** class for working with long integers and rational numbers with high precision. The fast multiplication of long integers in O(nlogn) using **FFT** (fast Fourier transform) is implemented
3) The **Geometry** class, which demonstrates how **Inheritance** and **Virtual functions** work in C++
4) The **Residue** class, which implements the ring of subtractions modulo N. By the example of this class we show how to work with templates in C++: In Compile-time it checks the module for simplicity, for the existence of the first-order root
5) Class **Matrix** for working with matrices over arbitrary fields. Matrix multiplication by **Strassen** method, addressing and determinant search operations are implemented. By the example of this class, the work with templates in C++ is demonstrated: in Compile-time there is a check for the possibility to multiply 2 matrices, to take the determinant
6) Implemented template class **Deque**, an analogue of the STL class with amortized running time O(1). The internal type iterator is also implemented. Push and pop operations **does not disable iterators** on elements. 

#include <stdio.h>

int main()
{
    long long low1, high1, low2, high2,t1,t2;
    __asm__ __volatile__("rdtsc": "=a" (low1), "=d" (high1));
    __asm__ __volatile__("rdtsc": "=a" (low2), "=d" (high2));
    
    t1 = low1 | (high1<<32);
    t2 = low2 | (high2<<32);
    printf("diff: %lld\n", t2-t1);
    return 0;
}


#include <stdio.h>

#include <termios.h>
#include "termops.h"

void set_icanon(int fd)
{
        struct termios term;
        tcgetattr(0, &term);
        term.c_lflag |= ICANON;
        tcsetattr(fd, TCSAFLUSH, &term);
}


void unset_icanon(int fd)
{
        struct termios term;
        tcgetattr(0, &term);
        term.c_lflag &= ~ICANON;
        tcsetattr(fd, TCSAFLUSH, &term);
}

__inline__ unsigned long long rdtsc(void)
{
    unsigned t1_high,t1_low;
    __asm__ volatile("rdtsc" : "=a" (t1_low), "=d" (t1_high));
    //__asm__ __volatile__("rdtsc": "=A" (t2));
    //printf("hey: %d\n", (t2-t1));
    //return t1;
    return t1_low | (((unsigned long long)t1_high) << 32);
}

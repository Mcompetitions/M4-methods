#ifndef MY_SETJMP2A_INCLUDED
#define MY_SETJMP2A_INCLUDED

struct my_jmpbuf {
  uint eip,esp;
};

#define ASM __asm__ volatile

int __inline my_setjmp( my_jmpbuf* regs ) {
  int r;
  int r0,r1,r2,r3,r4,r5,r6;

  ASM ("\
  xchg %%esp,%%eax; \
  addl $4,%%esp; \
  call 1f; \
1:   addl $11,0(%%esp); \
  movl %%eax,4(%%esp); \
  xchg %%esp,%%eax; \
  xorl %%eax,%%eax; \
  " : "=a"(r),"=r"(r0),"=r"(r1),"=r"(r2),"=r"(r3),"=r"(r4),"=r"(r5) : "a"(regs) : 
  );


  return r;
}

//__declspec(noreturn) 
void __inline my_jmp( my_jmpbuf* regs, int ) {
  ASM ("\
  mov 4(%%ebx),%%esp; \
  mov $1,%%eax; \
  jmp *0(%%ebx); \
  " :  : "b"(regs) : 
  );

}

typedef my_jmpbuf m_jmp_buf[1];
#define jmp_buf m_jmp_buf
#define longjmp my_jmp
#define setjmp  my_setjmp

#endif // MY_SETJMP2A_INCLUDED

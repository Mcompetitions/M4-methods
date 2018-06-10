#ifndef MY_SETJMP2_X64_INCLUDED
#define MY_SETJMP2_X64_INCLUDED

struct my_jmpbuf {
  qword rip,rsp;
};

#define ASM __asm__ volatile

int __inline my_setjmp( my_jmpbuf* regs ) {
  __int64 r;
  __int64 r0,r1,r2,r3,r4,r5,r6;
  __int64 q0,q1,q2,q3,q4,q5,q6,q7;

  ASM ("\
  xchg %%rsp,%%rax; \
  add $8,%%rsp; \
  call 1f; \
1:   addq $15,0(%%rsp); \
  mov %%rax,8(%%rsp); \
  xchg %%rsp,%%rax; \
  xor %%rax,%%rax; \
  " : "=a"(r),"=r"(r0),"=r"(r1),"=r"(r2),"=r"(r3),"=r"(r4),"=r"(r5),"=r"(q0),"=r"(q1),"=r"(q2),"=r"(q3),"=r"(q4),"=r"(q5),"=r"(q6),"=r"(q7) : "a"(regs) : 
  );


  return r;
}

//__declspec(noreturn) 
void __inline my_jmp( my_jmpbuf* regs, int ) {
  ASM ("\
  mov 8(%%rbx),%%rsp; \
  mov $1,%%rax; \
  jmp *0(%%rbx); \
  " :  : "b"(regs) : 
  );

}

typedef my_jmpbuf m_jmp_buf[1];
#define jmp_buf m_jmp_buf
#define longjmp my_jmp
#define setjmp  my_setjmp

#endif

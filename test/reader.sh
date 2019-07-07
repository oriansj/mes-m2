
MES_DEBUG=2 MES_BOOT=reader-0.scm out-glibc/mes
exit 0

#define NEW 0 gives:
symbol! -
number! 1
(- 1)

#define NEW 1 gives:
number! 0
number! 1
(0 1)

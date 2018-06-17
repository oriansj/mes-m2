#if !__M2_PLANET__
enum type_t {TCHAR, TCLOSURE, TCONTINUATION, TFUNCTION, TKEYWORD, TMACRO, TNUMBER, TPAIR, TREF, TSPECIAL, TSTRING, TSYMBOL, TVALUES, TVARIABLE, TVECTOR, TBROKEN_HEART};
#endif

struct scm {
  enum type_t type;
  SCM car;
  SCM cdr;
};

struct scm *g_cells;

int puts (char const *);
char const *itoa (int);
void *malloc (int);

#if __M2_PLANET__
#define struct_size 12
#define TYPE(x) ((x*struct_size)+g_cells)->type
#define CAR(x) ((x*struct_size)+g_cells)->car
#define CDR(x) ((x*struct_size)+g_cells)->cdr
#define VALUE(x) ((x*struct_size)+g_cells)->cdr
#else // !__M2_PLANET__
#define TYPE(x) (g_cells+x)->type
#define CAR(x) (g_cells+x)->car
#define CDR(x) (g_cells+x)->cdr
#define VALUE(x) (g_cells+x)->cdr
#endif // !__M2_PLANET__

int
main (int argc, char **argv)
{
  char *arena = malloc (3*sizeof (struct scm));
  g_cells = arena;

  TYPE (0) = TPAIR;
  CAR (0) = 11;
  CDR (0) = 12;

  int i = 1;
  TYPE (i) = 0;
  CAR (i) = 0;
  CDR (i) = 0;

  puts ("type 0:");
  puts (itoa (TYPE (0)));
  puts ("\n");

  puts ("setting type 2\n");
  if (argc > 1)
    TYPE (2) = TPAIR;
  
  puts ("type 0:");
  puts (itoa (TYPE (0)));
  puts ("\n");
  return TYPE (0);
}

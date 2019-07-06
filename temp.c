#include "mes.h"

extern struct scm* g_cells;
SCM GetSCM2(struct scm* a)
{
	return (a - g_cells);
}

struct scm* Getstructscm2(SCM a)
{
	return (a + g_cells);
}

struct scm* good2bad(struct scm* a)
{
	return (struct scm*)(a - g_cells);
}

struct scm* bad2good(struct scm* a)
{
	return ((SCM)a + g_cells);
}

SCM GetSCM(struct scm* a, struct scm* table)
{
	return(a - table);
}

extern struct scm* g_news;
struct scm* g2b(struct scm* a)
{
	return (struct scm*)(a - g_news);
}

#include "mes.h"

SCM GetSCM2(struct scm* a, struct scm* table)
{
	return (a - table);
}

struct scm* Getstructscm2(SCM a, struct scm* table)
{
	return a + table;
}

struct scm* good2bad(struct scm* a, struct scm* table)
{
	return (struct scm*)(a - table);
}

struct scm* bad2good(struct scm* a, struct scm* table)
{
	return ((SCM)a + table);
}

extern struct scm* g_news;
SCM GetSCM(struct scm* a)
{
	return(a - g_news);
}

struct scm* g2b(struct scm* a)
{
	return (struct scm*)(a - g_news);
}

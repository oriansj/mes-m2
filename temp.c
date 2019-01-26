#include "mes.h"

SCM GetSCM(struct scm* a)
{
	return (SCM) a;
}

struct scm* Getstructscm(SCM a)
{
	return (struct scm*) a;
}

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
	return (GetSCM(a) + table);
}

#include "mes.h"

SCM GetSCM(struct scm* a)
{
	return (SCM) a;
}

struct scm* Getstructscm(SCM a)
{
	return (struct scm*) a;
}

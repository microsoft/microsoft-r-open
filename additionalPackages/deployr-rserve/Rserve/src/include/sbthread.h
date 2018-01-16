/*****************************************************************\
 * sbthread - system-independent basic threads                   *
 * (C)Copyright 2001 Simon Urbanek                               *
 *---------------------------------------------------------------*
 * Supported platforms: unix w pthread, Win32                    *
\*****************************************************************/

#ifndef __SBTHREAD_H__
#define __SBTHREAD_H__


#include <pthread.h>

#define decl_sbthread void *
#define sbthread_result(A) (void *)(A)
#define sbthread_mutex pthread_mutex_t

sbthread_mutex *sbthread_create_mutex() {
  pthread_mutex_t lm=PTHREAD_MUTEX_INITIALIZER;
  pthread_mutex_t *m=(pthread_mutex_t *)malloc(sizeof(pthread_mutex_t));
  memcpy(m,&lm,sizeof(pthread_mutex_t));
  return m;
};

#define sbthread_lock_mutex(M) pthread_mutex_lock(M);
#define sbthread_unlock_mutex(M) pthread_mutex_unlock(M);
void sbthread_destroy_mutex(sbthread_mutex *m) {
  pthread_mutex_destroy(m); free(m);
};

int sbthread_create(void * (thr)(void *), void *par) {
  pthread_t Thread;
  pthread_attr_t ThreadAttr;

  pthread_attr_init(&ThreadAttr);
  pthread_attr_setdetachstate(&ThreadAttr,PTHREAD_CREATE_DETACHED);
  return pthread_create(&Thread,&ThreadAttr,*thr,par);
};



#endif /* __SBTHREAD_H__ */

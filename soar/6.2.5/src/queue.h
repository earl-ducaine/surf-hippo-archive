/*
 * $Id: queue.h,v 1.3 1994/08/23 10:37:07 portelli Exp $
 * $Log: queue.h,v $
 * Revision 1.3  1994/08/23  10:37:07  portelli
 * For 6.2.4
 *
 * Revision 1.2  1993/11/21  17:06:56  soarhack
 * 6.1.1 checkin
 *
 * Revision 1.1  1993/06/17  20:47:48  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:23:21  jtraub
 * 6.1_checkin
 *
 * Revision 9.2  1993/05/10  19:07:18  jtraub
 * added RCS header information
 *
 */

/************************************************************************/
/*                                                                      */
/*  Queue ADT: Access functions specified in queue.c.  Basic data       */
/*             structure is a linked list of cons cells.  Pointers to   */
/*             the head and tail of the linked list indicate the front  */
/*             and rear of the queue, respectively.  Items are added to */
/*             the rear (only) and removed from the front (only) so we  */
/*             have a strict FIFO queueing discipline.                  */
/*                                                                      */
/************************************************************************/

#ifndef _QUEUE_H_INCLUDED
#define _QUEUE_H_INCLUDED

#include "soar.h"

typedef struct queue_struct {
  cons * front;
  cons * rear;
} queue;

extern void queue_create (queue * * q);
extern void queue_add (queue * q, void * item);
extern bool queue_delete (queue * q, void * * item);
extern bool queue_is_empty (queue * q);

#endif /* _QUEUE_H_INCLUDED */


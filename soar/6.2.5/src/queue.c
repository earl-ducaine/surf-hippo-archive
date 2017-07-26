/*
 * $Id: queue.c,v 1.5 1994/11/23 16:40:34 rempel Exp $
 * $Log: queue.c,v $
 * Revision 1.5  1994/11/23  16:40:34  rempel
 * for 6.2.4
 *
 * Revision 1.4  1994/08/23  10:36:55  portelli
 * For 6.2.4
 *
 * Revision 1.3  1994/07/01  15:57:30  portelli
 * For 6.2.2
 *
 * Revision 1.2  1993/11/21  17:06:27  soarhack
 * 6.1.1 checkin
 *
 * Revision 1.1  1993/06/17  20:47:47  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:23:09  jtraub
 * 6.1_checkin
 *
 * Revision 9.2  1993/05/10  19:08:03  jtraub
 * added RCS header information
 *
 */

/************************************************************************/
/*                                                                      */
/*  Queue ADT: Basic data structure specified in queue.h.  Access fns   */
/*             include queue_create, queue_add, queue_delete, and       */
/*             queue_is_empty.  Queue element types of (void *) are     */
/*             used so that any data type can be added to a queue.      */
/*                                                                      */
/************************************************************************/

#include "soar.h"
#include "queue.h"


/*----------------------------------------------------------------------*/
/*                                                                      */
/*  queue_create: Creates a queue.  This is required before a queue     */
/*                can be used.                                          */
/*                                                                      */
/*----------------------------------------------------------------------*/

void queue_create (queue * * q)
{
  (*q)        = (queue *) malloc (sizeof(queue));
  (*q)->front = NIL;
  (*q)->rear  = NIL;
}


/*----------------------------------------------------------------------*/
/*                                                                      */
/*  queue_add: Add an item to a queue.  Before adding an item to the    */
/*             queue, a pointer to the elt must be cast to void.        */
/*             That enables any data type to be stored in the queue.    */
/*                                                                      */
/*             !! A check should be added to see if any memory is left  */
/*                to allocate the next queue position.                  */
/*----------------------------------------------------------------------*/

void queue_add (queue * q, void * item)
{
  cons * new_cons;
  cons * prev_rear;

  allocate_cons(&new_cons);
  new_cons->first = item;

  /* MVP 6-24-94  initialization fix from Karl */
  new_cons->rest  = NIL;

  prev_rear = q->rear;
  q->rear = new_cons;

  if (prev_rear == NIL) {
    q->front = new_cons;
  } else {
    prev_rear->rest = new_cons;
  }
}


/*----------------------------------------------------------------------*/
/*                                                                      */
/*  queue_delete: Delete an item from a queue.  The item deleted from   */
/*                The queue is returned via the pointer reference given */
/*                in the item parameter.  TRUE is returned if an item   */
/*                was deleted and FALSE is returned if no items were    */
/*                present in the queue.                                 */
/*                                                                      */
/*----------------------------------------------------------------------*/

bool queue_delete (queue * q, void * * item)
{
  void * item_deleted;
  cons * cons_to_delete;

  if (q->front == NIL) {
    print("\nError -- attempt to remove element from an empty queue.\n");
    return FALSE;
  }

  cons_to_delete = q->front;
  item_deleted   = q->front->first;
  q->front       = q->front->rest;

  free_cons(cons_to_delete);

  if (q->front == NIL) {
    q->rear = NIL;
  }

  *item = item_deleted;

  return TRUE;
}


/*----------------------------------------------------------------------*/
/*                                                                      */
/*  queue_is_empty:  Returns TRUE is queue is empty, FALSE otherwise.   */
/*                                                                      */
/*----------------------------------------------------------------------*/

bool queue_is_empty (queue * q) 
{
  return q->front == NIL;
}


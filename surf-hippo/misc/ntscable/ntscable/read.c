	/* %W% %G% */

	/*-------------------------------------------------------------
	|							      |
	|	PROGRAM:  ntscable                                    |
	|							      |
	|	MODULE:   %M%                                         |
	|							      |
	|	MACHINE:  Sun 3/60                                    |
	|							      |
	|	STARTED:  20-APR-89        BY:  J.C. Wathey           |
	|							      |
	|	REVISED:  %G%         BY:  JCW                   |
	|							      |
	|	STATUS:      incomplete or untested		      |
	|                    compiles; partly tested		      |
	|                    runs; revisions in progress	      |
	|                 -> runs; stable version		      |
	|							      |
	|       CONTAINS: routine read_neuron() and related utility   |
	|                 routines                                    |
	|                                                             |
	|       COMPILE:                                              |
	|                                                             |
	|       (use makefile)                                        |
	|                                                             |
	-------------------------------------------------------------*/

/*--------------------------------------- HEADER FILES --------------*/
#include "tf.h"
#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <ctype.h>
#include <errno.h>
#include "sets.h"
#include "ntscable.h"		       /* global defs & declarations */

			       	/* stack size is increased by this 
				amount at every detection of imminent
				stack overflow */
#define STACK_SIZE_INCREMENT	30

static int			stack_size = 0;
static TREE_POINT	     ** stack = (TREE_POINT **) NULL;
static TREE_POINT	     ** stack_ptr = (TREE_POINT **) NULL;

static char eutectic_hdr[] =
" Point  Type  Tag      X       Y       Z  Thick   Name    Attachment";

static char douglas_2d_hdr[] = "branchID br.ord. seglth segdiam term";
static char douglas_3d_hdr[] = "3d";

static char nevin_hdr[] = " */ ";

/*-------------------------------------------------------------------*/
int read_neuron(fp, neuron_ptr)

	/* Reads morphological description of a neuron from an ASCII
	file generated by any of several digitizing programs (see
	source_syntax in ntscable.h for a complete list).  Stores this 
	description in dynamic memory under the appropriate fields of 
	*neuron_ptr.  Gives messages and returns TRUE if error; 
	otherwise returns FALSE. */

    FILE      * fp; 
    NEURON    * neuron_ptr; 

{
				       /*----- functions called -----*/
    void outline_to_3D_list(),
	 measure_soma();

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int 	error,
		file_is_eutectic,
		file_is_douglas_2d,
		file_is_douglas_3d,
		file_is_nevin,
		line_number = 0;

    char      * filename;

    char	line[BUFSIZ];

				       /*----- start function -------*/

						 /* initialize stack */
    stack_ptr = stack;

    filename = neuron_ptr->input_filename;

    					 /* initialize comment field */
    if (strsave(&(neuron_ptr->comment), ""))
	exit(TRUE);

	      /* Read and store leading comment lines until the header 
	      line is encountered.  Header line determines which 
	      parsing protocol will be used.  */

    while( !(error = !fgets(line, BUFSIZ, fp)) 
	   && 
	   !(file_is_eutectic=!strncmp( eutectic_hdr,
					line,
					strlen(eutectic_hdr)))
	   && 
	   !(file_is_nevin=!strncmp(nevin_hdr,line,strlen(nevin_hdr)))
	   && 
	   !(file_is_douglas_2d=!strncmp( douglas_2d_hdr, 
					  line, 
					  strlen(douglas_2d_hdr)))
	   && 
	   !(file_is_douglas_3d=!strncmp( douglas_3d_hdr, 
					  line, 
					  strlen(douglas_3d_hdr)))) {

   	if (stradd(&(neuron_ptr->comment), line))
	    exit(TRUE);

	line_number++;
    }
					    /* count the header line */
    line_number++;

    if (error)
	fprintf(stderr, 
	"read_neuron: error reading %s at line %d\n", 
	filename, line_number);
    else if (file_is_eutectic)
	error = read_eutectic(fp, neuron_ptr, line_number);
    else if (file_is_douglas_2d)
	error = read_douglas_2d(fp, neuron_ptr, line_number);
    else if (file_is_douglas_3d)
	error = read_douglas_3d(fp, neuron_ptr, line_number);
    else if (file_is_nevin)
	error = read_nevin(fp, neuron_ptr, line_number);

    if (error)
	return(TRUE);

    if (!(neuron_ptr->soma_3D_list)) {
	outline_to_3D_list( neuron_ptr );
    }

    if (neuron_ptr->num_soma_3D_points < 2) {
	fprintf(stderr, "ntscable: WARNING: not enough soma points\n");
    }
    else {
    	measure_soma( neuron_ptr );
    }

    return(FALSE);
}

/*-------------------------------------------------------------------*/
int stack_is_empty()

	/* Returns TRUE if branch stack is empty; FALSE otherwise. */

{
    return( stack_ptr == stack );
}

/*-------------------------------------------------------------------*/
void push_branch(tree_ptr)

	/* Pushes tree_ptr (which points to an unresolved branch point)
        onto the stack.  Expands the stack by STACK_SIZE_INCREMENT if 
	stack overflow is imminent. */

    TREE_POINT * tree_ptr;

{
				       /*----- extern variables -----*/
    extern int		stack_size;
    extern TREE_POINT **stack;
    extern TREE_POINT **stack_ptr;
				       /*----- start function -------*/

    if (stack_ptr == stack + stack_size) {
	if (reallocate( (char **) &stack,
			(stack_size + STACK_SIZE_INCREMENT)
			* sizeof(TREE_POINT *),
			"branch stack" ))
	    exit(TRUE);
	stack_ptr = stack + stack_size;
	stack_size += STACK_SIZE_INCREMENT;
    }

    * (stack_ptr++) = tree_ptr;
}


/*-------------------------------------------------------------------*/
int pop_branch(tree_2ptr)

	/* Pops into *tree_2ptr the most recently encountered 
        unresolved branch point	from the stack.  Returns 
        TRUE if stack underflow occurs; FALSE otherwise. */

    TREE_POINT ** tree_2ptr;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/
    extern int		stack_size;
    extern TREE_POINT **stack;
    extern TREE_POINT **stack_ptr;

				       /*----- local  variables -----*/
    int		error;
				       /*----- start function -------*/

    if (error = (stack_ptr == stack))
	fprintf(stderr,"pop_branch: stack underflow\n");
    else
	*tree_2ptr = *(--stack_ptr);

    return(error);
}


/*-------------------------------------------------------------------*/
int start_new_tree(neuron_ptr, point_number, tree_2ptr, 
			  neurite_2ptr, x,y,z,diam )

	/* Starts a new tree of neurites, rooted at the soma of 
	*neuron_ptr, by allocating the first TREE_POINT.  Memory for 
        this first point is allocated either at neuron_ptr->neurites, 
        if this is NULL on entry, or at *neurite_2ptr->branch.  Returns 
        pointers to the newly allocated point in both *tree_2ptr and 
        *neurite_2ptr.  Uses the remaining arguments to fill the other 
        fields of the new TREE_POINT.  Returns TRUE if error; FALSE 
        otherwise. */

    NEURON      * neuron_ptr;
    int 	  point_number;
    TREE_POINT ** tree_2ptr,
	       ** neurite_2ptr;
    double	  x,y,z,diam;

{
				       /*----- functions called -----*/

				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int		  error;
    TREE_POINT	* ptr;
				       /*----- start function -------*/

    if (error = allocate_bytes( (char **) &ptr,
				      sizeof(TREE_POINT),
					"new tree point" ))
	return(error);

    if (neuron_ptr->neurites)
	(*neurite_2ptr)->branch = ptr;
    else 
	neuron_ptr->neurites = ptr;

    *tree_2ptr = *neurite_2ptr = ptr;
    memset( (char *) ptr, '\0', sizeof(TREE_POINT) );
    ptr->x = x;
    ptr->y = y;
    ptr->z = z;
    ptr->diam = diam;
    ptr->point_number = point_number;
    error = strsave(&(ptr->point_label), "");

    return(error);
}				       /*----- end function ---------*/

/*-------------------------------------------------------------------*/
int append_to_tree( point_number, tree_2ptr, x,y,z,diam )

	/* Appends a new TREE_POINT to an existing tree of neurites.
	Memory for this new point is allocated either at 
        (*tree_2ptr)->next, if this is NULL on entry, or at 
        (*tree_2ptr)->branch.  Returns a pointer to the newly allocated 
        point in *tree_2ptr.   Uses the remaining arguments to fill the 
        other fields of the new TREE_POINT.  Returns TRUE if error; 
        FALSE otherwise. */

    TREE_POINT ** tree_2ptr;
    int 	  point_number;
    double	  x,y,z,diam;

{
				       /*----- functions called -----*/
    double distance();
				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int		error;
    TREE_POINT *ptr;
				       /*----- start function -------*/

    if (error = allocate_bytes( (char **) &ptr,
				  sizeof(TREE_POINT),
				    "new tree point" ))
	return(error);

    memset( (char *) ptr, '\0', sizeof(TREE_POINT) );

    if ((*tree_2ptr)->next)
	(*tree_2ptr)->branch = ptr;
    else 
	(*tree_2ptr)->next = ptr;

    ptr->previous = *tree_2ptr;
    
    *tree_2ptr = ptr;
    ptr->x = x;
    ptr->y = y;
    ptr->z = z;
    ptr->diam = diam;
    ptr->point_number = point_number;

    ptr->length = distance( ptr->previous->x, x,
			    ptr->previous->y, y,
			    ptr->previous->z, z );

    error = strsave(&(ptr->point_label), "");
		
    return(error);
}				       /*----- end function ---------*/


/*-------------------------------------------------------------------*/
double distance( x1, x2, y1, y2, z1, z2 )

	/* Calculates distance between 2 points in 3-space. */

    double	x1, x2, y1, y2, z1, z2;


{
				       /*----- functions called -----*/
   double sqrt();
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
				       /*----- start function -------*/
    return( sqrt(  (x1-x2)*(x1-x2)
		   +
		   (y1-y2)*(y1-y2)  
		   +
		   (z1-z2)*(z1-z2) ));
}				       /*----- end function ---------*/


/*-------------------------------------------------------------------*/
void append_soma_3D_point( neuron_ptr, x, y, z, diam )

	/* Expands the list of 3D points at neuron_ptr->soma_3D_list,
	increments neuron_ptr->num_elements, and fills the fields of
	the newly allocated element with the last 4 arguments. */

    NEURON    * neuron_ptr;
    double	x,
		y,
		z,
		diam;
{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
    XYZD     * ptr;
				       /*----- start function -------*/

    if (reallocate( (char **) &(neuron_ptr->soma_3D_list),
		    ++(neuron_ptr->num_soma_3D_points)
		    * sizeof(XYZD), 
		    "soma 3D list" ))
	exit(TRUE);

    ptr = neuron_ptr->soma_3D_list + neuron_ptr->num_soma_3D_points -1;

    ptr->x = x;
    ptr->y = y;
    ptr->z = z;
    ptr->diam = diam;
}

/*-------------------------------------------------------------------*/
void append_soma_outline_point( soma_outline_ptr,
				point_number,
				x, y, z )

	/* Expands the circularly-linked list of soma outline points 
	at *soma_outline_ptr and fills the fields of the newly 
	allocated element with the last 4 arguments. */

    SOMA_POINT ** soma_outline_ptr;
    int	 	  point_number;
    double	  x,
		  y,
		  z;
{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/

    SOMA_POINT * new_point = (SOMA_POINT *) NULL,
	       * former_end_of_list;
				       /*----- start function -------*/

    if (reallocate( (char **) &new_point,
		    sizeof(SOMA_POINT), 
		    "soma outline" ))
	exit(TRUE);

    new_point->x = x;
    new_point->y = y;
    new_point->z = z;
    new_point->point_number = point_number;

    if (*soma_outline_ptr) {

	former_end_of_list = (*soma_outline_ptr)->previous;

	(*soma_outline_ptr)->previous = new_point;
	former_end_of_list->next      = new_point;
	new_point->previous           = former_end_of_list;
	new_point->next 	      = *soma_outline_ptr;
    }
    else {
	new_point->next = new_point->previous = new_point;
	*soma_outline_ptr = new_point;
    }
}

/*-------------------------------------------------------------------*/
void free_soma_outline( soma_outline_ptr )

	/* Frees the circularly-linked list of soma outline points 
	at *soma_outline_ptr and sets *soma_outline_ptr to NULL. */

    SOMA_POINT ** soma_outline_ptr;
{
				       /*----- functions called -----*/
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/

    SOMA_POINT * ptr, 
	       * next;
				       /*----- start function -------*/

    if (*soma_outline_ptr) {
						 /* break the circle */

	(*soma_outline_ptr)->previous->next = (SOMA_POINT *) NULL;

	ptr = *soma_outline_ptr;

	while (ptr) {
	    next = ptr->next;
	    free( (char *) ptr);
	    ptr = next;
	}
	
	*soma_outline_ptr = (SOMA_POINT *) NULL;
    }
}

/*-------------------------------------------------------------------*/
static void outline_to_3D_list( neuron_ptr )

	/* Constructs under neuron_ptr->soma_3D_list a list of 3D 
	points (x,y,z,diam) for the soma from the list of soma 
	outline points at neuron_ptr->soma_outline.  The diameters
	are all measured perpendicular to the x-axis.  There is one
	3D point for each outline point.  The 3D points are ordered
	from leftmost (least x) to rightmost (greatest x) in the list.
	Does nothing if there are fewer than 3 points in the soma 
	outline. */

    NEURON    * neuron_ptr;

{
				       /*----- functions called -----*/
    void outline_to_3D_point();
				       /*----- extern variables -----*/
				       /*----- local  variables -----*/
    double	 mean_z;

    SOMA_POINT * leftmost_point,
	       * forward_ptr,
	       * backward_ptr,
	       * ptr;
				       /*----- start function -------*/

    if (neuron_ptr->num_soma_outline_points < 3)
	return;
    					    /* average z-coordinates */
    if (neuron_ptr->average_outline_z_coords) {

        mean_z = 0.0;

	ptr = neuron_ptr->soma_outline;

	do {
	    mean_z += ptr->z;
	    ptr = ptr->next;
	} while (ptr != neuron_ptr->soma_outline);

        mean_z /= neuron_ptr->num_soma_outline_points;

	do {
	    ptr->z = mean_z;
	    ptr = ptr->next;
	} while (ptr != neuron_ptr->soma_outline);

    }

    					      /* find leftmost point */
    leftmost_point = neuron_ptr->soma_outline;

    for ( ptr = neuron_ptr->soma_outline->next;
	  ptr != neuron_ptr->soma_outline;
	  ptr = ptr->next ) {

	if (leftmost_point->x > ptr->x )
	    leftmost_point = ptr;
    }

    outline_to_3D_point( neuron_ptr,
		         leftmost_point,
		         leftmost_point,
		         leftmost_point );

    forward_ptr  = leftmost_point->next;
    backward_ptr = leftmost_point->previous;

    while( forward_ptr != backward_ptr ) {

	if (forward_ptr->x > backward_ptr->x) {
            while( forward_ptr != backward_ptr 
		   &&
	    	   forward_ptr->x > backward_ptr->x) {

    		outline_to_3D_point( neuron_ptr,
		     		     forward_ptr->previous,
		     		     backward_ptr,
		     		     forward_ptr );

		backward_ptr = backward_ptr->previous;
	    }
	}
	else {
            while( forward_ptr != backward_ptr 
		   &&
	    	   forward_ptr->x <= backward_ptr->x) {

    		outline_to_3D_point( neuron_ptr,
		     		     backward_ptr->next,
		     		     forward_ptr,
		     		     backward_ptr );

		forward_ptr = forward_ptr->next;
	    }
	}
    }

    outline_to_3D_point( neuron_ptr,
		         forward_ptr,
		         forward_ptr,
		         forward_ptr );
}

/*-------------------------------------------------------------------*/
static void outline_to_3D_point( neuron_ptr,
		     	         left,
		     	         middle,
		     	         right )

	/* Expands the list of 3D points at neuron_ptr->soma_3D_list,
	increments neuron_ptr->num_elements, and fills the fields of
	the newly allocated element by constructing a chord across
	the soma outline from middle to the line segment between left 
	and right.  Does nothing if, on entry, middle->x is less
	than the x value of the most recently appended 3D point.
	Also does nothing if the newly calculated point is identical
	to the one most recently appended to the list. */

    NEURON         * neuron_ptr;
    SOMA_POINT     * left,
	           * middle,
	           * right;

{
				       /*----- functions called -----*/
    double	distance();
    void	append_soma_3D_point();
				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    double	x,
		y,
		z,
		diam,
    		interpolated_y,
		interpolated_z,
		y_slope,
		z_slope;

    XYZD      * ptr,
	      * previous_point = (XYZD *) NULL;

				       /*----- start function -------*/
    if ( neuron_ptr->num_soma_3D_points
	 &&
	 (previous_point = neuron_ptr->soma_3D_list 
	  +
          neuron_ptr->num_soma_3D_points - 1)->x > middle->x )
	return;


    if (right->x == left->x) {
	x = left->x;
	y = (left->y + right->y)/2.0;
	z = (left->z + right->z)/2.0;
	diam = distance( 0.0, 0.0, 
			 left->y, right->y,
			 left->z, right->z );
    }
    else {
	x = middle->x;

	y_slope = (right->y - left->y) / (right->x - left->x);
	z_slope = (right->z - left->z) / (right->x - left->x);

	interpolated_y = left->y + y_slope * (middle->x - left->x);
	interpolated_z = left->z + z_slope * (middle->x - left->x);

	y = (interpolated_y + middle->y)/2.0;
	z = (interpolated_z + middle->z)/2.0;

	diam = distance( 0.0, 0.0, 
			 middle->y, interpolated_y,
			 middle->z, interpolated_z );
    }

    				     /* if this point is a duplicate, 
    				     don't append it to list */
    if ( previous_point
	 &&
	 previous_point->x == x
	 &&
	 previous_point->y == y
	 &&
	 previous_point->z == z
	 &&
	 previous_point->diam == diam )
	return;

    append_soma_3D_point( neuron_ptr, x, y, z, diam );
}

/*-------------------------------------------------------------------*/
static void measure_soma( neuron_ptr )

	/* Calculates diameter and length of cylinder having same
	surface area (excluding cylinder ends) and length as the 
	soma shape specified by neuron_ptr->soma_3D_list.  Stores
	the results in neuron_ptr->soma_length and neuron_ptr->
	soma_diam.  Does nothing if both of these are nonzero on
	entry, or if the calculated soma length is zero. */

    NEURON * neuron_ptr;

{
				       /*----- functions called -----*/
    double	distance();
				       /*----- extern variables -----*/

				       /*----- local  variables -----*/
    int	    	i;
    double    	weighted_diameter,
		interval_length,
	    	length;
    XYZD      * list;

				       /*----- start function -------*/

    weighted_diameter = length = 0.0;
    list = neuron_ptr->soma_3D_list;

    for (i=1; i<neuron_ptr->num_soma_3D_points; i++) {
	interval_length = distance( list[i].x, list[i-1].x,
			    	    list[i].y, list[i-1].y,
			    	    list[i].z, list[i-1].z );
	length += interval_length;
	weighted_diameter += (list[i].diam + list[i-1].diam)
			     * interval_length;
    }
    if (length > 0) {
        neuron_ptr->soma_diam = weighted_diameter /(2.0*length);
        neuron_ptr->soma_length = length;
    }
    neuron_ptr->soma_area = length * neuron_ptr->soma_diam * M_PI;
}


/*
 * $Id: x_resources.c,v 1.5 1994/08/23 10:40:10 portelli Exp $
 * $Log: x_resources.c,v $
 * Revision 1.5  1994/08/23  10:40:10  portelli
 * For 6.2.4
 *
 * Revision 1.4  1994/06/02  20:20:09  rempel
 * fixed some ModSAF stuff for Karl
 *
 * Revision 1.3  1993/11/24  14:12:11  portelli
 * fixed 6.1.1 bug
 *
 * Revision 1.2  93/11/21  22:33:21  portelli
 * 6.1.1 checkin
 * 
 * Revision 1.1  93/11/21  16:21:25  soarhack
 * initial checkin
 */

/* 
 * Reads in X resource information for Soar interface.  
 * This is based on an example from the Xlib programming
 * manual of O'Reilly.
 */

#include "soar.h"

#ifdef USE_X_DISPLAY

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <X11/Xresource.h>
#if !(XlibSpecificationRelease == 5)  /* Define XPointer for X11R4 */
  typedef caddr_t XPointer;
#endif

#include <stdio.h>

#include <ctype.h>

#ifdef __hpux
#define _INCLUDE_POSIX_SOURCE
#endif /* __hpux */
#include <pwd.h>
#ifdef __hpux
#undef _INCLUDE_POSIX_SOURCE
#endif /* __hpux */

#include <stdlib.h>

#include "x_resources.h"

soar_x_default_record soar_x_defaults;

static XrmDatabase commandlineDB, rDB;

char myDisplayName[256];

char		*calcName;

int		screen_number;
Visual		*visual;
Colormap	colormap;
XFontStruct	*theFont;
Cursor 		theCursor;
Window 		calcWin;
Window 		iconWin;
Window 		dispWin;

int		foreground;
int		background;
int             border;

/* Command line options table.  Only resources are entered here...there is a
   pass over the remaining options after XrmParseCommand is let loose. 
   We don't do anything with many of these resources, but the program is 
   ready for expansion */

#define GEOMETRY	"*geometry"
#define ICONGEOMETRY	"*iconGeometry"
#define UNSIGNED	"*unsigned"
#define BASE		"*base"
#define ICONSTARTUP	"*iconStartup"

static int opTableEntries = 25;
static XrmOptionDescRec opTable[] = {
{"-unsigned",	UNSIGNED,	XrmoptionNoArg,		(XPointer) "off"},
{"-x",		BASE,		XrmoptionNoArg,		(XPointer) "16"},
{"-hex",	BASE,		XrmoptionNoArg,		(XPointer) "16"},
{"-dec",	BASE,	 	XrmoptionNoArg,		(XPointer) "10"},
{"-oct",	BASE,	 	XrmoptionNoArg,		(XPointer) "8"},
{"-binary",	BASE,	 	XrmoptionNoArg,		(XPointer) "2"},
{"-geometry",   GEOMETRY,       XrmoptionSepArg,        (XPointer) NULL},
{"-iconGeometry", ICONGEOMETRY, XrmoptionSepArg,        (XPointer) NULL},
{"-iconic",     ICONSTARTUP,    XrmoptionNoArg,         (XPointer) "on"},
{"-background", "*background",  XrmoptionSepArg,        (XPointer) NULL},
{"-bg",         "*background",  XrmoptionSepArg,        (XPointer) NULL},
{"-fg",         "*foreground",  XrmoptionSepArg,        (XPointer) NULL},
{"-foreground", "*foreground",  XrmoptionSepArg,        (XPointer) NULL},
{"-xrm",        NULL,           XrmoptionResArg,        (XPointer) NULL},
{"-display",    ".display",     XrmoptionSepArg,        (XPointer) NULL},
/* remainder not currently supported: */
{"-bd",        "*borderColor",  XrmoptionSepArg,        (XPointer) NULL},
{"-bordercolor", "*borderColor", XrmoptionSepArg,       (XPointer) NULL},
{"-borderwidth", ".borderWidth", XrmoptionSepArg,       (XPointer) NULL},
{"-bw",        ".borderWidth",  XrmoptionSepArg,        (XPointer) NULL},
{"-fn",        "*font",         XrmoptionSepArg,        (XPointer) NULL},
{"-font",      "*font",         XrmoptionSepArg,        (XPointer) NULL},
{"-name",      ".name",         XrmoptionSepArg,        (XPointer) NULL},
{"-title",     ".title",        XrmoptionSepArg,        (XPointer) NULL},
};

static char *getHomeDir( dest )
char *dest;
{
    int uid;
    extern char *getenv();
    extern int getuid();
/*    extern struct passwd *getpwuid(); */
    struct passwd *pw;
    register char *ptr;

    if ((ptr = getenv("HOME")) != NULL) {
        (void) strcpy(dest, ptr);

    } else {
        if ((ptr = getenv("USER")) != NULL) {
            pw = getpwnam(ptr);
        } else {
            uid = getuid();
            pw = getpwuid(uid);
        }
        if (pw) {
            (void) strcpy(dest, pw->pw_dir);
        } else {
            *dest = '\0';
        }
    }
    return dest;
}


/*
 * Get program's and user's defaults
 */
mergeDatabases()
{
    XrmDatabase homeDB, serverDB, applicationDB;

    char filenamebuf[1024];
    char *filename = &filenamebuf[0];
    char *environment;
    char *classname = "Soar";
    char name[255];

    (void) strcpy(name, "/usr/lib/X11/app-defaults/");
    (void) strcat(name, classname);
    /* get application defaults file, if any */
    applicationDB = XrmGetFileDatabase(name);
    (void) XrmMergeDatabases(applicationDB, &rDB);

    /* MERGE server defaults, these are created by xrdb, loaded as a
     * property of the root window when the server initializes, and
     * loaded into the display structure on XOpenDisplay.  If not defined,
         * use .Xdefaults  */
    if (XResourceManagerString(soar_display) != NULL) {
        serverDB = XrmGetStringDatabase(XResourceManagerString(soar_display));
    } else {
        /* Open .Xdefaults file and merge into existing data base */
        (void) getHomeDir(filename);
        (void) strcat(filename, "/.Xdefaults");

        serverDB = XrmGetFileDatabase(filename);
    }
    XrmMergeDatabases(serverDB, &rDB);

    /* Open XENVIRONMENT file, or if not defined, the ~/.Xdefaults,
         * and merge into existing data base */
    if ((environment = getenv("XENVIRONMENT")) == NULL) {
        int len;
        environment = getHomeDir(filename);
        (void) strcat(environment, "/.Xdefaults-");
        len = strlen(environment);
        (void) gethostname(environment + len, 1024 - len);
    }
    homeDB = XrmGetFileDatabase(environment);
    XrmMergeDatabases(homeDB, &rDB);

    /* command line takes precedence over everything */
    XrmMergeDatabases(commandlineDB, &rDB);
}


/*
 * Get command line options
 */
parseOpenDisp (argc, argv)
int *argc;
register char *argv[];
{

    XrmValue value;
    char *str_type[20];

    myDisplayName[0] = '\0';

    XrmParseCommand(&commandlineDB, opTable, opTableEntries,
            argv[0], argc, argv);

    /*
     * Check for any arguments left
     */

    if (*argc != 1) 
        Usage();

    /* get display now, because we need it to get other databases*/
    if (XrmGetResource(commandlineDB, "soar.display",
            "Soar.Display", str_type, &value) == True) {
        (void) strncpy(myDisplayName, value.addr, (int) value.size);
    }

    /*
     * Open display 
     */

    if (!(soar_display = XOpenDisplay(myDisplayName))) {
        fprintf(stderr, "%s: Can't open display '%s'\n",
                argv[0], XDisplayName(myDisplayName));
	fprintf (stderr, "Is your DISPLAY environment variable set properly?\n");
	fprintf (stderr, "Terminating program...\n");

        exit(1);
    }

    screen_number = DefaultScreen(soar_display);
    visual = DefaultVisual(soar_display, screen_number);
    colormap = DefaultColormap(soar_display, screen_number);
}


void soar_X_resource (char * agent_class, 
		      char * agent_name,
		      char * resource_name,
		      char * resource_value,
		      bool monitor,
		      char * resource_default)
{
  char resource_path_name[100];
  char resource_path_class[100];
  char * c;
  char *str_type[20];
  XrmValue value;

  (void) strcpy(resource_path_name, "soar.");
  if (monitor)
    (void) strcat(resource_path_name, "monitor.");
  if (agent_class[0])
    {
      (void) strcat(resource_path_name, agent_class);
      (void) strcat(resource_path_name, ".");
    }

  if (agent_name[0])
    {
      (void) strcat(resource_path_name, agent_name);
      (void) strcat(resource_path_name, ".");
    }

  (void) strcat(resource_path_name, resource_name);

  (void) strcpy(resource_path_class, resource_path_name);

  for (c = resource_path_name; *c; c++)  /* Make sure lower case */
    {
      if (   (c == resource_path_class)
	  || (*(c-1) == '.'))
	*c = tolower(*c);
    }

  for (c = resource_path_class; *c; c++)  /* Make sure capitalized */
    {
      if (   (c == resource_path_class)
	  || (*(c-1) == '.'))
	*c = toupper(*c);
    }

  if (XrmGetResource(rDB, 
		     resource_path_name, resource_path_class,
		     str_type, &value) 
      == True) 
    {
      (void) strncpy(resource_value, value.addr, (int) value.size);
    } else {
      (void) strcpy(resource_value, resource_default);
    }
}


void soar_get_all_X_resources (char * class, char * name, bool monitor)
{
  soar_X_resource (class, name, "display",     soar_x_defaults.display,     monitor, "");
  soar_X_resource (class, name, "geometry",    soar_x_defaults.geometry,    monitor, "500x250+0+0");
  soar_X_resource (class, name, "font",        soar_x_defaults.font_name,   monitor, "6x13");
  soar_X_resource (class, name, "background",  soar_x_defaults.background,  monitor, "White");
  soar_X_resource (class, name, "foreground",  soar_x_defaults.foreground,  monitor, "Black");
  soar_X_resource (class, name, "borderWidth", soar_x_defaults.borderwidth, monitor, "4");
  soar_X_resource (class, name, "borderColor", soar_x_defaults.bordercolor, monitor, "White");
  soar_X_resource (class, name, "initialState",soar_x_defaults.initialstate,monitor, "NormalState");
}

/*
 * Print message to stderr and exit
 */
Usage ()
{
    (void) fprintf (stderr, "%s: [-display <display>] [-geometry <geometrystring>]\n",
            calcName ? calcName : "soar");
    exit (1);
}


void
print_results (void)
{ printf("\nSoar X defaults:\n");
  printf("  Display:     %s\n", soar_x_defaults.display);
  printf("  Geometry:    %s\n", soar_x_defaults.geometry);
  printf("  Font:        %s\n", soar_x_defaults.font_name);
  printf("  Background:  %s\n", soar_x_defaults.background);
  printf("  Foreground:  %s\n", soar_x_defaults.foreground);
  printf("  Borderwidth: %s\n", soar_x_defaults.borderwidth);
  printf("  Bordercolor: %s\n", soar_x_defaults.bordercolor);
  printf("  InitialState:%s\n", soar_x_defaults.initialstate);
}


void
soar_init_x_resources(int argc, char * control_name, char * argv[])
{
    /* so we can use the resource manager data merging functions */
    XrmInitialize();

    /* parse command line first so we can open display, store any
     * options in a database  */
    parseOpenDisp (&argc, argv);

    /* get server defaults, program defaults, .Xdefaults, command 
     * line, etc. and merge them */
    mergeDatabases();

    /* extract values from database for use */
    soar_get_all_X_resources("", control_name, FALSE);
}


/* Here for only for debugging as a standalone file ...

Display * soar_display;

int
main (int argc, char * argv[])
{
  soar_init_x_resources(argc, argv);
  print_results();
  XCloseDisplay(soar_display);
}

*/


#endif /* USE_X_DISPLAY */



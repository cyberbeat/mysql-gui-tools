#include <gtk/gtk.h>
#include "gtksourceview/gtktextregion.h"


int
main (int argc, char **argv)
{
	GtkTextBuffer *buffer;
	GtkTextRegion *region, *intersection;
	GtkTextIter iter1, iter2;
	gint i;
	
#define NUM_OPS 23
	
	gint ops [NUM_OPS][3] = {
		/* add/remove a 0-length region */
		{  1,  5,  5 },
		{ -1,  5,  5 },
		/* add a region */
		{  1,  5, 10 },
		/* add two adjacent regions */
		{  1,  3,  5 },
		{  1, 10, 12 },
		/* remove all */
		{ -1,  1, 15 },
		/* add two separate regions */
		{  1,  5, 10 },
		{  1, 15, 20 },
		/* join them */
		{  1,  7, 17 },
		/* remove from the middle */
		{ -1, 10, 15 },
		/* exactly remove a subregion */
		{ -1, 15, 20 },
		/* try to remove an adjacent region */
		{ -1, 10, 20 },
		/* try to remove an adjacent region */
		{ -1,  0,  5 },
		/* add another separate */
		{  1, 15, 20 },
		/* join with excess */
		{  1,  0, 25 },
		/* do two holes */
		{ -1,  5, 10 },
		{ -1, 15, 20 },
		/* remove the middle subregion */
		{ -1,  8, 22 },
		/* add the subregion we just removed */
		{  1, 10, 15 },
		/* remove the middle subregion */
		{ -1,  3, 17 },
		/* add the subregion we just removed */
		{  1, 10, 15 },
		/* remove the middle subregion */
		{ -1,  2, 23 },
		/* add the subregion we just removed */
		{  1, 10, 15 },
	};
	
#define NUM_INTERSECTS 5

	gint inter [NUM_INTERSECTS][2] = {
		{  0, 25 },
		{ 10, 15 },
		{  8, 17 },
		{  1, 24 },
		{  3,  7 }
	};
		
	gtk_init (&argc, &argv);
	
	buffer = gtk_text_buffer_new (NULL);
	region = gtk_text_region_new (buffer);

	gtk_text_buffer_get_start_iter (buffer, &iter1);
	gtk_text_buffer_insert (buffer, &iter1, "This is a test of GtkTextRegion", -1);

	for (i = 0; i < NUM_OPS; i++) {
		gchar *op_name;
		
		gtk_text_buffer_get_iter_at_offset (buffer, &iter1, ops [i][1]);
		gtk_text_buffer_get_iter_at_offset (buffer, &iter2, ops [i][2]);

		if (ops [i][0] > 0) {
			op_name = "added";
			gtk_text_region_add (region, &iter1, &iter2);
		} else {
			op_name = "deleted";
			gtk_text_region_substract (region, &iter1, &iter2);
		}
		g_print ("%s %d-%d\n", op_name, ops [i][1], ops [i][2]);

		gtk_text_region_debug_print (region);
	}
	
	for (i = 0; i < NUM_INTERSECTS; i++) {
		gtk_text_buffer_get_iter_at_offset (buffer, &iter1, inter [i][0]);
		gtk_text_buffer_get_iter_at_offset (buffer, &iter2, inter [i][1]);

		g_print ("intersect %d-%d\n", inter [i][0], inter [i][1]);
		intersection = gtk_text_region_intersect (region, &iter1, &iter2);
		if (intersection) {
			gtk_text_region_debug_print (intersection);
			gtk_text_region_destroy (intersection, TRUE);
		} else {
			g_print ("no intersection\n");
		}
	}
	
	gtk_text_region_destroy (region, TRUE);
	g_object_unref (buffer);
	
	return 0;
}


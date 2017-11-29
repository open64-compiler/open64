//CMD: $(CXX) -O2 -fno-exceptions $(SOURCE) -o $(TARGET)
#include <wchar.h>
#include <stdio.h>


//extern FILE *opnfil(int);
//extern int clsrmfil(int);

FILE *opnfil(int i){return NULL;};
int clsrmfil(int i){return i;};
FILE *fptr;
int ok = 1;
void verify(char *, wchar_t *);
void trim(wchar_t *);
/*--------------------------------------------------------------------*/

int main(void)
{
	int ret, arg;
	printf("ISO 9899-1: 4.6.2.4.1 The fwprintf function\n");

	;	/* block 02 */

	/* d conversion with negative argument. */
	if (ok) {
		arg = -102;
		if ((ret = fwprintf(fptr,L"%d",arg)) <= 0) {
			
			printf("fwprintf() with %%d returned %d.\n",ret);
		}
		else
			verify("%d",L"-102");
	}
	else {
		
		printf("setup failure.\n");
	}
/*--------------------------------------------------------------------*/
	;	/* block 02 */

	/* d conversion with negative argument. */
	if (ok) {
		arg = -102;
		if ((ret = fwprintf(fptr,L"%d",arg)) <= 0) {
			
			printf("fwprintf() with %%d returned %d.\n",ret);
		}
		else
			verify("%d",L"-102");
	}
	else {
		
		printf("setup failure.\n");
	}
/*--------------------------------------------------------------------*/
	;	/* block 02 */

	/* d conversion with negative argument. */
	if (ok) {
		arg = -102;
		if ((ret = fwprintf(fptr,L"%d",arg)) <= 0) {
			
			printf("fwprintf() with %%d returned %d.\n",ret);
		}
		else
			verify("%d",L"-102");
	}
	else {
		
		printf("setup failure.\n");
	}

/*--------------------------------------------------------------------*/
	;	/* block 03 */

	/* d conversion with large negative argument. */
	if (ok) {
		arg = -32766;
		if ((ret = fwprintf(fptr,L"%d",arg)) <= 0) {
			
			printf("fwprintf() with %%d returned %d.\n",ret);
		}
		else
			verify("%d",L"-32766");
	}
	else {
		
		printf("setup failure.\n");
	}

/*--------------------------------------------------------------------*/
	/* block 04 */

	/* d conversion with field width. */
	if (ok) {
		arg = 20766;
		if ((ret = fwprintf(fptr,L"%5d",arg)) <= 0) {
			
			printf("fwprintf() with %5d returned %d.\n",ret);
		}
		else
			verify("%5d",L"20766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 05 */

	/* d conversion with field width, space padding. */
	if (ok) {
		arg = 44;
		if ((ret = fwprintf(fptr,L"%5d",arg)) <= 0) {
			
			printf("fwprintf() with %%5d returned %d.\n",ret);
		}
		else
			verify("%5d",L"   44");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 06 */

	/* d conversion with field width, space padding, large arg. */
	if (ok) {
		arg = 32766;
		if ((ret = fwprintf(fptr,L"%6d",arg)) <= 0) {
			
			printf("fwprintf() with %%6d returned %d.\n",ret);
		}
		else
			verify("%6d",L" 32766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 07 */

	/* d conversion with large field width, space padding. */
	if (ok) {
		arg = 6;
		if ((ret = fwprintf(fptr,L"%23d",arg)) <= 0) {
			
			printf("fwprintf() with %%23d returned %d.\n",ret);
		}
		else
			verify("%23d",L"                      6");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 08 */

	/* d conversion with field with, space padding, and *
	 * negative argument.				    */
	if (ok) {
		arg = -32;
		if ((ret = fwprintf(fptr,L"%5d",arg)) <= 0) {
			
			printf("fwprintf() with %%5d returned %d.\n",ret);
		}
		else
			verify("%5d",L"  -32");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 09 */

	/* d conversion with asterisk field width. */
	if (ok) {
		int fld = 5;
		arg = 20766;
		if ((ret = fwprintf(fptr,L"%*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%*d returned %d.\n",ret);
		}
		else
			verify("*d",L"20766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 10 */

	/* d conversion with asterisk field width, space padding. */
	if (ok) {
		int fld = 5;
		arg = 44;
		if ((ret = fwprintf(fptr,L"%*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%*d returned %d.\n",ret);
		}
		else
			verify("%*d",L"   44");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 11 */

	/* d conversion with asterisk field width, space padding, large arg. */
	if (ok) {
		int fld = 6;
		arg = 32766;
		if ((ret = fwprintf(fptr,L"%*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%*d returned %d.\n",ret);
		}
		else
			verify("%*d",L" 32766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 12 */

	/* d conversion with asterisk large field width, space padding. */
	if (ok) {
		int fld = 23;
		arg = 6;
		if ((ret = fwprintf(fptr,L"%*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%*d returned %d.\n",ret);
		}
		else
			verify("%*d",L"                      6");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 13 */

	/* d conversion with asterisk field with, space padding, and *
	 * negative argument.				    */
	if (ok) {
		int fld = 5;
		arg = -32;
		if ((ret = fwprintf(fptr,L"%*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%*d returned %d.\n",ret);
		}
		else
			verify("%*d",L"  -32");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 14 */

	/* d conversion with asterisk field width, 0 flag. */
	if (ok) {
		int fld = 5;
		arg = 20766;
		if ((ret = fwprintf(fptr,L"%0*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%0*d returned %d.\n",ret);
		}
		else
			verify("0*d",L"20766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 15 */

	/* d conversion with asterisk field width, 0 flag. */
	if (ok) {
		int fld = 5;
		arg = 44;
		if ((ret = fwprintf(fptr,L"%0*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%0*d returned %d.\n",ret);
		}
		else
			verify("%0*d",L"00044");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 16 */

	/* d conversion with asterisk field width, 0 flag, large arg. */
	if (ok) {
		int fld = 6;
		arg = 32766;
		if ((ret = fwprintf(fptr,L"%0*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%0*d returned %d.\n",ret);
		}
		else
			verify("%0*d",L"032766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 17 */

	/* d conversion with asterisk large field width, 0 flag. */
	if (ok) {
		int fld = 23;
		arg = 6;
		if ((ret = fwprintf(fptr,L"%0*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%0*d returned %d.\n",ret);
		}
		else
			verify("%0*d",L"00000000000000000000006");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 18 */

	/* d conversion with asterisk field with, 0 flag, and *
	 * negative argument.				    */
	if (ok) {
		int fld = 5;
		arg = -32;
		if ((ret = fwprintf(fptr,L"%0*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%0*d returned %d.\n",ret);
		}
		else
			verify("%0*d",L"-0032");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 19 */

	/* d conversion with field width and 0 flag. */
	if (ok) {
		arg = 20766;
		if ((ret = fwprintf(fptr,L"%05d",arg)) <= 0) {
			
			printf("fwprintf() with %%05d returned %d.\n",ret);
		}
		else
			verify("%05d",L"20766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 20 */

	/* d conversion with field width and 0 flag. */
	if (ok) {
		arg = 44;
		if ((ret = fwprintf(fptr,L"%05d",arg)) <= 0) {
			
			printf("fwprintf() with %%05d returned %d.\n",ret);
		}
		else
			verify("%05d",L"00044");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 21 */

	/* d conversion with field width, 0 flag, large arg. */
	if (ok) {
		arg = 32766;
		if ((ret = fwprintf(fptr,L"%06d",arg)) <= 0) {
			
			printf("fwprintf() with %%06d returned %d.\n",ret);
		}
		else
			verify("%06d",L"032766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 22 */

	/* d conversion with large field width, 0 flag. */
	if (ok) {
		arg = 6;
		if ((ret = fwprintf(fptr,L"%023d",arg)) <= 0) {
			
			printf("fwprintf() with %%023d returned %d.\n",ret);
		}
		else
			verify("%023d",L"00000000000000000000006");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 23 */

	/* d conversion with field with, 0 flag, and negative arg. */
	if (ok) {
		arg = -32;
		if ((ret = fwprintf(fptr,L"%05d",arg)) <= 0) {
			
			printf("fwprintf() with %%05d returned %d.\n",ret);
		}
		else
			verify("%05d",L"-0032");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 24 */

	/* d conversion with field width, left justified. */
	if (ok) {
		arg = 20766;
		if ((ret = fwprintf(fptr,L"%-5d",arg)) <= 0) {
			
			printf("fwprintf() with %%-5d returned %d.\n",ret);
		}
		else
			verify("%-5d",L"20766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 25 */

	/* d conversion with field width, space padding, left justified. */
	if (ok) {
		arg = 44;
		if ((ret = fwprintf(fptr,L"%-5d",arg)) <= 0) {
			
			printf("fwprintf() with %%-5d returned %d.\n",ret);
		}
		else
			verify("%-5d",L"44   ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 26 */

	/* d conversion with field width, space padding, large arg
	left justified. */
	if (ok) {
		arg = 32766;
		if ((ret = fwprintf(fptr,L"%-6d",arg)) <= 0) {
			
			printf("fwprintf() with %%-6d returned %d.\n",ret);
		}
		else
			verify("%-6d",L"32766 ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 27 */

	/* d conversion with large field width, space padding
	left justified. */
	if (ok) {
		arg = 6;
		if ((ret = fwprintf(fptr,L"%-23d",arg)) <= 0) {
			
			printf("fwprintf() with %%-23d returned %d.\n",ret);
		}
		else
			verify("%-23d",L"6                      ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 28 */

	/* d conversion with field with, space padding, left *
	 * justified, and a negative argument.		     */
	if (ok) {
		arg = -32;
		if ((ret = fwprintf(fptr,L"%-5d",arg)) <= 0) {
			
			printf("fwprintf() with %%-5d returned %d.\n",ret);
		}
		else
			verify("%-5d",L"-32  ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 29 */

	/* d conversion with asterisk field width, left justified. */
	if (ok) {
		int fld = 5;
		arg = 20766;
		if ((ret = fwprintf(fptr,L"%-*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%-*d returned %d.\n",ret);
		}
		else
			verify("-*d",L"20766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 30 */

	/* d conversion with asterisk field width, space padding, *
	   left justified.					  */
	if (ok) {
		int fld = 5;
		arg = 44;
		if ((ret = fwprintf(fptr,L"%-*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%-*d returned %d.\n",ret);
		}
		else
			verify("%-*d",L"44   ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 31 */

	/* d conversion with asterisk field width, space padding, *
	   left justified, and a large arg.			  */
	if (ok) {
		int fld = 6;
		arg = 32766;
		if ((ret = fwprintf(fptr,L"%-*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%-*d returned %d.\n",ret);
		}
		else
			verify("%-*d",L"32766 ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 32 */

	/* d conversion with asterisk large field width, space   *
	    padding, and left justified.			 */
	if (ok) {
		int fld = 23;
		arg = 6;
		if ((ret = fwprintf(fptr,L"%-*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%-*d returned %d.\n",ret);
		}
		else
			verify("%-*d",L"6                      ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 33 */

	/* d conversion with asterisk field with, space padding, left *
	 * justified, and a negative argument.			      */
	if (ok) {
		int fld = 5;
		arg = -32;
		if ((ret = fwprintf(fptr,L"%-*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%-*d returned %d.\n",ret);
		}
		else
			verify("%-*d",L"-32  ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 34 */

	/* d conversion with asterisk field width, 0 and - flags. */
	if (ok) {
		int fld = 5;
		arg = 20766;
		if ((ret = fwprintf(fptr,L"%-0*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%-0*d returned %d.\n",ret);
		}
		else
			verify("%-0*d",L"20766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 35 */

	/* d conversion with asterisk field width, 0 flag, *
	 * and left justified.				   */
	if (ok) {
		int fld = 5;
		arg = 44;
		if ((ret = fwprintf(fptr,L"%0-*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%0-*d returned %d.\n",ret);
		}
		else
			verify("%0-*d",L"44   ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 36 */

	/* d conversion with asterisk field width, 0 flag, large *
	 * argument, left justified.				 */
	if (ok) {
		int fld = 6;
		arg = 32766;
		if ((ret = fwprintf(fptr,L"%-0*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%-0*d returned %d.\n",ret);
		}
		else
			verify("%-0*d",L"32766 ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 37 */

	/* d conversion with asterisk large field width, 0 flag, *
	 * left justified.					 */
	if (ok) {
		int fld = 23;
		arg = 6;
		if ((ret = fwprintf(fptr,L"%0-*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%0-*d returned %d.\n",ret);
		}
		else
			verify("%0-*d",L"6                      ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 38 */

	/* d conversion with asterisk field with, 0 flag, left  *
	 * justified, and a negative argument.			*/
	if (ok) {
		int fld = 5;
		arg = -32;
		if ((ret = fwprintf(fptr,L"%-0*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%-0*d returned %d.\n",ret);
		}
		else
			verify("%0*d",L"-32  ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 39 */

	/* d conversion with field width and 0 flag, left justified. */
	if (ok) {
		arg = 20766;
		if ((ret = fwprintf(fptr,L"%0-5d",arg)) <= 0) {
			
			printf("fwprintf() with %%0-5d returned %d.\n",ret);
		}
		else
			verify("%0-5d",L"20766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 40 */

	/* d conversion with field width and 0 flag, left justified. */
	if (ok) {
		arg = 44;
		if ((ret = fwprintf(fptr,L"%-05d",arg)) <= 0) {
			
			printf("fwprintf() with %%-05d returned %d.\n",ret);
		}
		else
			verify("%-05d",L"44   ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 41 */

	/* d conversion with field width, 0 flag, large arg, left *
	 * justified.						  */
	if (ok) {
		arg = 32766;
		if ((ret = fwprintf(fptr,L"%0-6d",arg)) <= 0) {
			
			printf("fwprintf() with %%0-6d returned %d.\n",ret);
		}
		else
			verify("%0-6d",L"32766 ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 42 */

	/* d conversion with large field width, 0 flag, left justified. */
	if (ok) {
		arg = 6;
		if ((ret = fwprintf(fptr,L"%-023d",arg)) <= 0) {
			
			printf("fwprintf() with %%-023d returned %d.\n",ret);
		}
		else
			verify("%-023d",L"6                      ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 43 */

	/* d conversion with field with, 0 flag, left justified, and *
	 * a negative argument.					     */
	if (ok) {
		arg = -32;
		if ((ret = fwprintf(fptr,L"%0-5d",arg)) <= 0) {
			
			printf("fwprintf() with %%0-5d returned %d.\n",ret);
		}
		else
			verify("%0-5d",L"-32  ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 44 */

	/* d conversion with field width, + flag. */
	if (ok) {
		arg = 2076;
		if ((ret = fwprintf(fptr,L"%+5d",arg)) <= 0) {
			
			printf("fwprintf() with +%5d returned %d.\n",ret);
		}
		else
			verify("+%5d",L"+2076");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 45 */

	/* d conversion with field width, space padding, + flag. */
	if (ok) {
		arg = 44;
		if ((ret = fwprintf(fptr,L"%+5d",arg)) <= 0) {
			
			printf("fwprintf() with %%+5d returned %d.\n",ret);
		}
		else
			verify("%+5d",L"  +44");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 46 */

	/* d conversion with field width, space padding, large arg,  *
	 * and + flag.						     */
	if (ok) {
		arg = 32766;
		if ((ret = fwprintf(fptr,L"%+7d",arg)) <= 0) {
			
			printf("fwprintf() with %%+7d returned %d.\n",ret);
		}
		else
			verify("%+7d",L" +32766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 47 */

	/* d conversion with large field width, space padding, + flag. */
	if (ok) {
		arg = 6;
		if ((ret = fwprintf(fptr,L"%+23d",arg)) <= 0) {
			
			printf("fwprintf() with %%+23d returned %d.\n",ret);
		}
		else
			verify("%+23d",L"                     +6");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 48 */

	/* d conversion with field with, space padding, + flag, and *
	 * negative argument.					    */
	if (ok) {
		arg = -32;
		if ((ret = fwprintf(fptr,L"%+5d",arg)) <= 0) {
			
			printf("fwprintf() with %%+5d returned %d.\n",ret);
		}
		else
			verify("%+5d",L"  -32");
	}
	else {
		
		printf("setup failure.\n");
	}
	
/*--------------------------------------------------------------------*/
	;	/* block 49 */

	/* d conversion with asterisk field width, and + flag. */
	if (ok) {
		int fld = 6;
		arg = 20766;
		if ((ret = fwprintf(fptr,L"%+*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%+*d returned %d.\n",ret);
		}
		else
			verify("+*d",L"+20766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 50 */

	/* d conversion with asterisk field width, space padding, *
	 * and + flag.						  */
	if (ok) {
		int fld = 5;
		arg = 44;
		if ((ret = fwprintf(fptr,L"%+*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%+*d returned %d.\n",ret);
		}
		else
			verify("%+*d",L"  +44");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 51 */

	/* d conversion with asterisk field width, space padding,  *
	 * and + flag large arg.				   */
	if (ok) {
		int fld = 6;
		arg = 32766;
		if ((ret = fwprintf(fptr,L"%+*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%+*d returned %d.\n",ret);
		}
		else
			verify("%+*d",L"+32766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 52 */

	/* d conversion with asterisk large field width, space	*
	 * padding, and + flag.					*/
	if (ok) {
		int fld = 23;
		arg = 6;
		if ((ret = fwprintf(fptr,L"%+*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%+*d returned %d.\n",ret);
		}
		else
			verify("%+*d",L"                     +6");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 53 */

	/* d conversion with asterisk field with, space padding, +  *
	 * flag, and negative argument.				    */
	if (ok) {
		int fld = 5;
		arg = -32;
		if ((ret = fwprintf(fptr,L"%+*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%+*d returned %d.\n",ret);
		}
		else
			verify("%+*d",L"  -32");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 54 */

	/* d conversion with asterisk field width, 0 and + flags. */
	if (ok) {
		int fld = 6;
		arg = 20766;
		if ((ret = fwprintf(fptr,L"%0+*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%0+*d returned %d.\n",ret);
		}
		else
			verify("0+*d",L"+20766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 55 */

	/* d conversion with asterisk field width, 0 and + flags. */
	if (ok) {
		int fld = 5;
		arg = 44;
		if ((ret = fwprintf(fptr,L"%+0*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%+0*d returned %d.\n",ret);
		}
		else
			verify("%+0*d",L"+0044");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 56 */

	/* d conversion with asterisk field width, 0 flag, large arg
	 * and + flag. */
	if (ok) {
		int fld = 6;
		arg = 32766;
		if ((ret = fwprintf(fptr,L"%0+*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%0+*d returned %d.\n",ret);
		}
		else
			verify("%0+*d",L"+32766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 57 */

	/* d conversion with asterisk large field width, 0 and + flags. */
	if (ok) {
		int fld = 23;
		arg = 6;
		if ((ret = fwprintf(fptr,L"%+0*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%+0*d returned %d.\n",ret);
		}
		else
			verify("%+0*d",L"+0000000000000000000006");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 58 */

	/* d conversion with asterisk field with, 0 and + flags,  *
	 * and a negative argument.				  */
	if (ok) {
		int fld = 5;
		arg = -32;
		if ((ret = fwprintf(fptr,L"%+0*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%+0*d returned %d.\n",ret);
		}
		else
			verify("%+0*d",L"-0032");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 59 */

	/* d conversion with field width and 0 and + flags. */
	if (ok) {
		arg = 20766;
		if ((ret = fwprintf(fptr,L"%0+6d",arg)) <= 0) {
			
			printf("fwprintf() with %%0+6d returned %d.\n",ret);
		}
		else
			verify("%0+6d",L"+20766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 60 */

	/* d conversion with field width and 0 and + flags. */
	if (ok) {
		arg = 44;
		if ((ret = fwprintf(fptr,L"%+05d",arg)) <= 0) {
			
			printf("fwprintf() with %%+05d returned %d.\n",ret);
		}
		else
			verify("%+05d",L"+0044");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 61 */

	/* d conversion with field width, 0 and + flags, large arg. */
	if (ok) {
		arg = 32766;
		if ((ret = fwprintf(fptr,L"%0+6d",arg)) <= 0) {
			
			printf("fwprintf() with %%0+6d returned %d.\n",ret);
		}
		else
			verify("%0+6d",L"+32766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 62 */

	/* d conversion with large field width, 0 and + flags. */
	if (ok) {
		arg = 6;
		if ((ret = fwprintf(fptr,L"%+023d",arg)) <= 0) {
			
			printf("fwprintf() with %%+023d returned %d.\n",ret);
		}
		else
			verify("%+023d",L"+0000000000000000000006");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 63 */

	/* d conversion with field with, 0 flag, negative argument, *
	 * and 0 and + flags.					    */
	if (ok) {
		arg = -32;
		if ((ret = fwprintf(fptr,L"%0+5d",arg)) <= 0) {
			
			printf("fwprintf() with %%0+5d returned %d.\n",ret);
		}
		else
			verify("%0+5d",L"-0032");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 64 */

	/* d conversion with field width, left justified, + flag. */
	if (ok) {
		arg = 2766;
		if ((ret = fwprintf(fptr,L"%+-5d",arg)) <= 0) {
			
			printf("fwprintf() with %%+-5d returned %d.\n",ret);
		}
		else
			verify("%+-5d",L"+2766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 65 */

	/* d conversion with field width, space padding, left   *
	 * justified, and + flag. 				*/
	if (ok) {
		arg = 44;
		if ((ret = fwprintf(fptr,L"%-+5d",arg)) <= 0) {
			
			printf("fwprintf() with %%-+5d returned %d.\n",ret);
		}
		else
			verify("%-+5d",L"+44  ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 66 */

	/* d conversion with field width, space padding, large   *
	 * argument, left justified, and + flag.		 */
	if (ok) {
		arg = 32766;
		if ((ret = fwprintf(fptr,L"%+-6d",arg)) <= 0) {
			
			printf("fwprintf() with %%+-6d returned %d.\n",ret);
		}
		else
			verify("%+-6d",L"+32766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 67 */

	/* d conversion with large field width, space padding,  *
	 * left justified, and + flag.				*/
	if (ok) {
		arg = 6;
		if ((ret = fwprintf(fptr,L"%-+23d",arg)) <= 0) {
			
			printf("fwprintf() with %%-+23d returned %d.\n",ret);
		}
		else
			verify("%-+23d",L"+6                     ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 68 */

	/* d conversion with field with, space padding, left *
	 * justified, + flag,  and a negative argument.	     */
	if (ok) {
		arg = -32;
		if ((ret = fwprintf(fptr,L"%+-5d",arg)) <= 0) {
			
			printf("fwprintf() with %%+-5d returned %d.\n",ret);
		}
		else
			verify("%+-5d",L"-32  ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 69 */

	/* d conversion with asterisk field width, left justified,  *
	 * and + flag.						    */
	if (ok) {
		int fld = 7;
		arg = 20766;
		if ((ret = fwprintf(fptr,L"%-+*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%-+*d returned %d.\n",ret);
		}
		else
			verify("-+*d",L"+20766 ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 70 */

	/* d conversion with asterisk field width, space padding, *
	   left justified, and a + flag.			  */
	if (ok) {
		int fld = 5;
		arg = 44;
		if ((ret = fwprintf(fptr,L"%+-*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%+-*d returned %d.\n",ret);
		}
		else
			verify("%+-*d",L"+44  ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 71 */

	/* d conversion with asterisk field width, space padding, *
	   left justified, + flag, and a large arg.		  */
	if (ok) {
		int fld = 7;
		arg = 32766;
		if ((ret = fwprintf(fptr,L"%-+*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%-+*d returned %d.\n",ret);
		}
		else
			verify("%-+*d",L"+32766 ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 72 */

	/* d conversion with asterisk large field width, space  *
	 * padding, + flah, and left justified.			*/
	if (ok) {
		int fld = 23;
		arg = 6;
		if ((ret = fwprintf(fptr,L"%+-*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%+-*d returned %d.\n",ret);
		}
		else
			verify("%+-*d",L"+6                     ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 73 */

	/* d conversion with asterisk field with, space padding, left *
	 * justified, + flag, and a negative argument.		      */
	if (ok) {
		int fld = 5;
		arg = -32;
		if ((ret = fwprintf(fptr,L"%-+*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%-+*d returned %d.\n",ret);
		}
		else
			verify("%-+*d",L"-32  ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 74 */

	/* d conversion with asterisk field width, 0 flag, left  *
	 * justified, and a + flag.				 */
	if (ok) {
		int fld = 7;
		arg = 20766;
		if ((ret = fwprintf(fptr,L"%+-0*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%+-0*d returned %d.\n",ret);
		}
		else
			verify("%+-0*d",L"+20766 ");
	}
	else {
		
		printf("setup failure.\n");
	}

/*--------------------------------------------------------------------*/
	;	/* block 75 */

	/* d conversion with asterisk field width, 0 and + flags, *
	 * and left justified.				   	  */
	if (ok) {
		int fld = 6;
		arg = 44;
		if ((ret = fwprintf(fptr,L"%+0-*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %+%0-*d returned %d.\n",ret);
		}
		else
			verify("+%0-*d",L"+44   ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 76 */

	/* d conversion with asterisk field width, 0 and + flags, *
	 * large argument, left justified.			  */
	if (ok) {
		int fld = 7;
		arg = 32766;
		if ((ret = fwprintf(fptr,L"%-0+*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%-0+*d returned %d.\n",ret);
		}
		else
			verify("%-0+*d",L"+32766 ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 77 */

	/* d conversion with asterisk large field width, 0 and, *
	 * + flags, and left justified.				*/
	if (ok) {
		int fld = 23;
		arg = 6;
		if ((ret = fwprintf(fptr,L"%0+-*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%0+-*d returned %d.\n",ret);
		}
		else
			verify("%0+-*d",L"+6                     ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 78 */

	/* d conversion with asterisk field with, 0 and + flags, left *
	 * justified, and a negative argument.			      */
	if (ok) {
		int fld = 5;
		arg = -32;
		if ((ret = fwprintf(fptr,L"%-+0*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %%-+0*d returned %d.\n",ret);
		}
		else
			verify("%-+0*d",L"-32  ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 79 */

	/* d conversion with field width and 0 and + flags, left   *
	 * justified.						   */
	if (ok) {
		arg = 20766;
		if ((ret = fwprintf(fptr,L"%0-+6d",arg)) <= 0) {
			
			printf("fwprintf() with %%0+-6d returned %d.\n",ret);
		}
		else
			verify("%0-+6d",L"+20766");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 80 */

	/* d conversion with field width and 0 and + flags, left  *
	 * justified.						  */
	if (ok) {
		arg = 44;
		if ((ret = fwprintf(fptr,L"%-0+5d",arg)) <= 0) {
			
			printf("fwprintf() with %%-0+5d returned %d.\n",ret);
		}
		else
			verify("%-0+5d",L"+44  ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 81 */

	/* d conversion with field width, 0 and + fslag, large  *
	 * argument, and left justified.			*/
	if (ok) {
		arg = 32766;
		if ((ret = fwprintf(fptr,L"%0+-7d",arg)) <= 0) {
			
			printf("fwprintf() with %%0+-7d returned %d.\n",ret);
		}
		else
			verify("%0+-7d",L"+32766 ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 82 */

	/* d conversion with large field width, 0 and + flags, and *
	 * left justified.					   */
	if (ok) {
		arg = 6;
		if ((ret = fwprintf(fptr,L"%-0+23d",arg)) <= 0) {
			
			printf("fwprintf() with %%-0+23d returned %d.\n",ret);
		}
		else
			verify("%-0+23d",L"+6                     ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 83 */

	/* d conversion with field with, 0 and + flags, left *
	 * justified, and a negative argument.		     */
	if (ok) {
		arg = -32;
		if ((ret = fwprintf(fptr,L"%+0-5d",arg)) <= 0) {
			
			printf("fwprintf() with %%+0-5d returned %d.\n",ret);
		}
		else
			verify("%+0-5d",L"-32  ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 84 */

	/* d conversion with space flag.		     */
	if (ok) {
		arg = 32;
		if ((ret = fwprintf(fptr,L"% d",arg)) <= 0) {
			
			printf("fwprintf() with %% d returned %d.\n",ret);
		}
		else
			verify("% d",L" 32");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 85 */

	/* d conversion with space flag and field width. */
	if (ok) {
		arg = 2;
		if ((ret = fwprintf(fptr,L"% 4d",arg)) <= 0) {
			
			printf("fwprintf() with %% 4d returned %d.\n",ret);
		}
		else
			verify("% 4d",L"   2");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 86 */

	/* d conversion with space flag and asterisk field width. */
	if (ok) {
		int fld = 5;
		arg = 102;
		if ((ret = fwprintf(fptr,L"% *d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %% *d returned %d.\n",ret);
		}
		else
			verify("% *d",L"  102");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 87 */

	/* d conversion with space and - flags and field width. */
	if (ok) {
		arg = 102;
		if ((ret = fwprintf(fptr,L"%- 5d",arg)) <= 0) {
			
			printf("fwprintf() with %%- 5d returned %d.\n",ret);
		}
		else
			verify("%- 5d",L" 102 ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 88 */

	/* d conversion with space and - flags and asterisk field width. */
	if (ok) {
		int fld = 5;
		arg = 102;
		if ((ret = fwprintf(fptr,L"% -*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %% -*d returned %d.\n",ret);
		}
		else
			verify("% -*d",L" 102 ");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 89 */

	/* d conversion with space and + flags. */
	if (ok) {
		arg = 102;
		if ((ret = fwprintf(fptr,L"%+ d",arg)) <= 0) {
			
			printf("fwprintf() with %%+ d returned %d.\n",ret);
		}
		else
			verify("%+ d",L"+102");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 90 */

	/* d conversion with space flag and precision. */
	if (ok) {
		arg = 102;
		if ((ret = fwprintf(fptr,L"% .4d",arg)) <= 0) {
			
			printf("fwprintf() with %% .4d returned %d.\n",ret);
		}
		else
			verify("% .4d",L" 0102");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 91 */

	/* d conversion with space flag and asterisk precision. */
	if (ok) {
		int fld = 4;
		arg = 102;
		if ((ret = fwprintf(fptr,L"% .*d",fld,arg)) <= 0) {
			
			printf("fwprintf() with %% .*d returned %d.\n",ret);
		}
		else
			verify("% .*d",L" 0102");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 92 */

	/* d conversion with h flag. */
	if (ok) {
		short int arg1 = 236;
		if ((ret = fwprintf(fptr,L"%hd",arg1)) <= 0) {
			
			printf("fwprintf() with %%hd returned %d.\n",ret);
		}
		else
			verify("%hd",L"236");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 93 */

	/* d conversion with h and + flags. */
	if (ok) {
		short int arg1 = 236;
		if ((ret = fwprintf(fptr,L"%+hd",arg1)) <= 0) {
			
			printf("fwprintf() with %%+hd returned %d.\n",ret);
		}
		else
			verify("%+hd",L"+236");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 94 */

	/* d conversion with h and + flags. */
	if (ok) {
		short int arg1 = -236;
		if ((ret = fwprintf(fptr,L"%+hd",arg1)) <= 0) {
			
			printf("fwprintf() with %%+hd returned %d.\n",ret);
		}
		else
			verify("%+hd",L"-236");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 95 */

	/* d conversion with h flag. */
	if (ok) {
		unsigned short int arg1 = 236;
		if ((ret = fwprintf(fptr,L"%hd",arg1)) <= 0) {
			
			printf("fwprintf() with %%hd returned %d.\n",ret);
		}
		else
			verify("%hd",L"236");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 96 */

	/* d conversion with h and + flags. */
	if (ok) {
		unsigned short int arg1 = 236;
		if ((ret = fwprintf(fptr,L"%+hd",arg1)) <= 0) {
			
			printf("fwprintf() with %%+hd returned %d.\n",ret);
		}
		else
			verify("%+hd",L"+236");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 97 */

	/* d conversion with h and + flags. */
	if (ok) {
		unsigned short int arg1 = 236;
		if ((ret = fwprintf(fptr,L"%+hd",arg1)) <= 0) {
			
			printf("fwprintf() with %%+hd returned %d.\n",ret);
		}
		else
			verify("%+hd",L"+236");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 98 */

	/* d conversion with l flag. */
	if (ok) {
		long int arg1 = 236;
		if ((ret = fwprintf(fptr,L"%ld",arg1)) <= 0) {
			
			printf("fwprintf() with %%ld returned %d.\n",ret);
		}
		else
			verify("%ld",L"236");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 99 */

	/* d conversion with l and + flags. */
	if (ok) {
		long int arg1 = 236;
		if ((ret = fwprintf(fptr,L"%+ld",arg1)) <= 0) {
			
			printf("fwprintf() with %%+ld returned %d.\n",ret);
		}
		else
			verify("%+ld",L"+236");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 100 */

	/* d conversion with l and + flags. */
	if (ok) {
		long int arg1 = -236;
		if ((ret = fwprintf(fptr,L"%+ld",arg1)) <= 0) {
			
			printf("fwprintf() with %%+ld returned %d.\n",ret);
		}
		else
			verify("%+ld",L"-236");
	}
	else {
		
		printf("setup failure.\n");
	}

	
/*--------------------------------------------------------------------*/
	;	/* block 101 */

	/* d conversion with l flag. */
	if (ok) {
		unsigned long int arg1 = 236;
		if ((ret = fwprintf(fptr,L"%ld",arg1)) <= 0) {
			
			printf("fwprintf() with %%ld returned %d.\n",ret);
		}
		else
			verify("%ld",L"236");
	}
	else {
		
		printf("setup failure.\n");
	}

	
	
/*--------------------------------------------------------------------*/
	(void)clsrmfil(0);

	return 0;
}
/*--------------------------------------------------------------------*/

void verify(char *format, wchar_t *exp)
{ if (format)return;
}
/*--------------------------------------------------------------------*/
void trim(wchar_t *s)
{
	wint_t ech;
	size_t l;

	l = wcslen(s);
	for (;;) {
		ech = s[l - 1];
		if (ech == L'\n' || ech == L'\r') {
			s[--l] = L'\0';
		}
		else break;
	}
}


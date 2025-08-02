
#include <errno.h> //Error codes
#include <ctype.h> //for Ctrl vals
#include <stdio.h> //printf() and perror() 
#include <fcntl.h> // open(), 0_RDWR and O_CREAT
#include <stdlib.h> //for exit and atexit()
#include <string.h> //string used for realloc() and memcpy()
#include <unistd.h> //for read(), STDIN_FILENO, also ftruncate() and close()
#include <sys/types.h> //ssize_t comes from <sys/types.h>.
#include <stdarg.h> //used for editorSetStatusMessage(...)
#include <sys/ioctl.h>//Getting the window size the easy way IOCtl (which itself stands for Input/Output Control) Get WINdow SiZe.)
#include <time.h> //for displaying current time for the user
#include <termios.h> //for manipulating terminal I/O settings
//note a structure is a user defined data type that is used to group items of different types into a single type
//almost like object I guess (java king)
 
/*** DEFINES ***/
#define CTRL_KEY(k) ((k) & 0x1f)
#define DOCKERKILO_VERSION "0.0.1"
#define KILO_TAB_STOP 8
#define KILO_QUIT_TIMES 3

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

enum editorKey {
	BACKSPACE = 127,
  	ARROW_LEFT = 1000, //setting first constant to 1000, subsequent constants get incremental values
  	ARROW_RIGHT, //I.E 1001,1002
  	ARROW_UP,
  	ARROW_DOWN,
	DEL_KEY,
	HOME_KEY,
  	END_KEY,
  	PAGE_UP,
  	PAGE_DOWN
};

enum editorHighlight {
	HL_NORMAL = 0,
	HL_COMMENT,
	HL_MLCOMMENT,
	HL_KEYWORD1,
	HL_KEYWORD2,
	HL_STRING,
	HL_NUMBER,
	HL_MATCH
};
/*** DATA ***/
struct editorSyntax {
	char *filetype;
	char **filematch; //Arr of strings
	char **keywords;
	char *singleline_comment_start;
	char *multiline_comment_start;
  	char *multiline_comment_end;
	int flags;
};
typedef struct erow {
	int idx;
	int size;
	int rsize; //contains size of the contents of render
	char *chars;
	char *render;
	unsigned char *hl;
	int hl_open_comment;
}erow ;
struct editorConfig {
	int cx, cy;//Moving that cursor is crazy work lmao
	int rx;
	int rowoff;
	int coloff;
	int screenrows;
	int screencols;
	int numrows;
	erow *row;
	int dirty;
	char *filename;
	char statusmsg[80]; //status msg for the user
	time_t statusmsg_time; //time_t from time.h
	struct editorSyntax *syntax;
  	struct termios orig_termios;
};

struct editorConfig E; //Think like an object E that contains the termios and orig_termios variables within it
//termios from termios, used to store terminal settings, very important
//Orig_termios is used for storing original data so we can save on exit of the program

/*** filetypes ***/

char *C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };
char *C_HL_keywords[] = {
  "switch", "if", "while", "for", "break", "continue", "return", "else",
  "struct", "union", "typedef", "static", "enum", "class", "case",
  "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
  "void|", NULL
};
struct editorSyntax HLDB[] = {
	//HLDB stands for highlight database
  {
    "c",
    C_HL_extensions,
	C_HL_keywords,
	"//", "/*", "*/",
	HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
  },
};
//Defining constant to store the length of the DLDB array
#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/*** prototypes ***/
void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));

/*** TERMINAL  ***/
void die(const char *s){
	//Clear the screen on exit
	write(STDOUT_FILENO, "\x1b[2J", 4);
  	write(STDOUT_FILENO, "\x1b[H", 3);

	//method handles fatal errors prints string s on exit
	perror(s);
	exit(1);
}
void disableRawMode(){
	//disabling the raw mode by setting attributes to the original terminal
	//STDIN_FILENO is a file descriptor for standard input
	//TCSAFLUSH apply changes after all pending output is written and discard unread input
	//&orig_termios is a pointer to the saved original settings (C pointers FTW)
	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
    die("tcsetattr");
}
int editorReadKey() {
  	int nread;
  	char c;
  	while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
    	if (nread == -1 && errno != EAGAIN) die("read");
  	}
	/*pressing an arrow key sends multiple bytes as input to our program. 
	 These bytes are in the form of an escape sequence
	 that starts with '\x1b', '[', followed by an 'A', 'B', 'C', or 'D' 
	 depending on which of the four arrow keys was pressed.*/
  	
	
	//If we read an escape character,we read two more bytes into seq immediately.
	/*otherwise we look to see if the escape sequence is an arrow key escape sequence
	if it is we just return the corresponding wasd character.*/
  	if (c == '\x1b') {
		char seq[3];
		if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
		if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';
		if (seq[0] == '[') {
		if (seq[1] >= '0' && seq[1] <= '9') {
			if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
			if (seq[2] == '~') {
			switch (seq[1]) {
				case '1': return HOME_KEY;
				case '3': return DEL_KEY;
				case '4': return END_KEY;
				case '5': return PAGE_UP;
				case '6': return PAGE_DOWN;
				case '7': return HOME_KEY;
				case '8': return END_KEY;
			}
			}
		} else {
			switch (seq[1]) {
			case 'A': return ARROW_UP;
			case 'B': return ARROW_DOWN;
			case 'C': return ARROW_RIGHT;
			case 'D': return ARROW_LEFT;
			case 'H': return HOME_KEY;
			case 'F': return END_KEY;
			}
		}
		} else if (seq[0] == 'O') {
		switch (seq[1]) {
			case 'H': return HOME_KEY;
			case 'F': return END_KEY;
		}
		}
		return '\x1b';
	} else {
		return c;
  }
}
void enableRawMode() {
	//tcgetattr gets the terminal attributes from file descriptor and stores in orig_termios pointer
    tcgetattr(STDIN_FILENO, &E.orig_termios);//set the terminal attributes

	//use this to register our disableRawMode() function to be called auto when program exit
	//leave it the way we found it. even if we are exiting on a CTRL_C
	atexit(disableRawMode);
    
	//Store the original terminal attributes in global var (making copy before changing everything)
	struct termios raw = E.orig_termios;

	//Note c_iflag is ctrl input flag which records which keypress is pressed lol
	//disable ctrl-s and ctrl-q which freeze/unfreeze our beut of a program
	//While pressing CTRL, we can type everything but m, which is being read as 10 bytes (not 13 like we expect)
	//Because CTRL-J already produces 10, and the enter key does aswell, ICRNL correct so ctrl-m == 13
	/*BRKINT: prevents break condition from causing SIGINT.

	ICRNL: disables conversion of carriage return (\r) to newline (\n).

	INPCK: disables parity check (used for serial).

	ISTRIP: disables stripping 8th bit.

	IXON: disables software flow control (Ctrl-S/Ctrl-Q).*/
  	raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  	raw.c_oflag &= ~(OPOST);//turn off output processing
	raw.c_cflag |= (CS8);//misc flag
	//c_lflag is for local flags, ICANON allows us to turn off canonical mode (READING BYTE TO BYTE FINALLY not line by line)
	//The Icanon flag will let us exit on pressing of q, not when we press enter
	//The Isig flag lets us read ctrl-c and ctrl-z
	//IEXTEN disable ctrl-v
	raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);//modifying the struct by hand
  	
	//vmin/vtim both from termios, indexes into the c_cc field which are control characters
	//Vmin sets minimum # of bytes of input needed before read() can return (set to 0 to read() returns as soon as there is any input to be read)
	//VTIME value sets max amount time to wait before read() returns
	raw.c_cc[VMIN] = 0;
	raw.c_cc[VTIME] = 1;

	//TCSAFLUSH tells when to apply the change, waiting for the pending output to be written to terminal (discarding any undread input)
	tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);//passing the modified struct bake into the method to write the new terminal attributes back out
    //struct termios, tcgetattr(), tcsetattr(), ECHO, and TCSAFLUSH all come from <termios.h>
	//this function applies all the settings we applied to the terminal 
	
	/*The Echo feature causes each key you type to be printed to the terminal so you can see what your typing
	  Very useful in canonical mode but gets in the way in raw mode so we turn it off*/  
	if(tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == 1) die("tcsetattr");
}
int getCursorPosition(int *rows, int *cols){
	char buf[32];//our buffer
	unsigned int i = 0;
	if(write(STDOUT_FILENO, "\x1b[6n",4) != 4) return -1;
		while (i < sizeof(buf) - 1) {
	    if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
	    if (buf[i] == 'R') break;
    	i++;
  	}
  	buf[i] = '\0';
  	if (buf[0] != '\x1b' || buf[1] != '[') return -1;
  	if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;
  	return 0;
}
int getWindowSize(int *rows, int *cols) {
  	struct winsize ws; //comes from <sys/ioctl.h>
	//IOCtl (which itself stands for Input/Output Control) Get WINdow SiZe.)

	//Basically, get the window size using the function, if this function fails, we will return -1;
	//If the function succeeds it plays the # rows/columns into the winsize construct lololol (I'm going insane a little )
  	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {

		/*As you can see, there is no easy way to get the cursor the bottom of the screen
		  We are just sending the cursor there by using a large value of 999 to ensure it reaches teh bottom right of the screen!*/
		if(write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
		/*The C and B commands are documented to stop the cursor from going past the edge of the screen*/
    	return getCursorPosition(rows,cols); 
  	} else {
    	*cols = ws.ws_col;
    	*rows = ws.ws_row;
    	return 0;
  	}
}

/*** syntax highlighting  ***/
int is_separator(int c){
	/*is the passed in char considered a "seperater character? "*/
	return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}
void editorUpdateSyntax(erow *row){
	/*realloc needed memory since might be new row or row might be bitgger than the last time we highlighted it*/
	row->hl = realloc(row->hl, row->rsize);
	/*Notice that the size of the hl array is the same as the render array so we use rsize as the amount
	of mem to allocate for hl*/
	/*Use memset to set all chars in HL_NORMAL by default, before looping through the characters and setting hte digits to HL_NUMBER*/
	memset(row->hl, HL_NORMAL, row->rsize); //comes from string.h

	if(E.syntax == NULL) return;

	//Keywords
	char **keywords = E.syntax->keywords;

	//Comments
	char *scs = E.syntax->singleline_comment_start;
	char *mcs = E.syntax->multiline_comment_start;
  	char *mce = E.syntax->multiline_comment_end;
	
	int scs_len = scs ? strlen(scs):0;
	int mcs_len = mcs ? strlen(mcs) : 0;
  	int mce_len = mce ? strlen(mce) : 0;


	//keeping track if previous char was a seperator character. initialised to 1 init as start of line is always seperator
	int prev_sep = 1;
	//Keeping track if inside a string.
	int in_string = 0;
	int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);

	int i = 0;
	while (i < row->rsize){
		char c = row->render[i];
		unsigned char prev_hl = (i > 0) ? row->hl[i-1] : HL_NORMAL;

		/*using strncmp() to check if this character is the start of a single line comment, if so then we simply memset()
		the whole rest of the line with HL_COMMENT and break out of the syntax highlight loop. whole line commented (BOOM)*/
		if (scs_len && !in_string && !in_comment) {
			if(!strncmp(&row->render[i],scs,scs_len)){//strncmp comes from string.j
				memset(&row->hl[i], HL_COMMENT, row->rsize-i);
				break;
			}
		}

		//MultiLine Comments
		if (mcs_len && mce_len && !in_string) {
			if (in_comment) {
					row->hl[i] = HL_MLCOMMENT;
					if (!strncmp(&row->render[i], mce, mce_len)) {
					memset(&row->hl[i], HL_MLCOMMENT, mce_len);
					i += mce_len;
					in_comment = 0;
					prev_sep = 1;
					continue;
				} else {
					i++;
					continue;
				}
			} else if (!strncmp(&row->render[i], mcs, mcs_len)) {
				memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
				i += mcs_len;
				in_comment = 1;
				continue;
			}
		}

		//Strings
		if(E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
			if(in_string){
				row->hl[i] = HL_STRING;
				if (c == '\\' && i + 1 < row->rsize) {
					row->hl[i + 1] = HL_STRING;
					i += 2; //Consume String
					continue;
				}
				if(c == in_string) in_string = 0;
				i++;
				prev_sep = 1;
				continue;
			} else {
				if(c == '"' || c == '\'') {
					in_string = c;
					row->hl[i] = HL_STRING;
					i++;
					continue;
				}
			}
		}
		if(E.syntax->flags & HL_HIGHLIGHT_NUMBERS){
		    if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
        		(c == '.' && prev_hl == HL_NUMBER)) {
					//. char comes after a character that we just hihglighted as a nubmer will now be considered #
				row->hl[i] = HL_NUMBER;
				i++;
				prev_sep = 0;
				continue;
			}
		}

		//Keywords
		if(prev_sep){
			int j;
			for(j=0; keywords[j];j++){
				int klen = strlen(keywords[j]);
				int kw2 = keywords[j][klen -1] == '|';
				if(kw2) klen--;
				/*use strncmp() to check if keyword exists at curr position in the text
				and also check to see if a separator character comes after the keyword, since \0 is considred separator char*/
				if(!strncmp(&row->render[i], keywords[j], klen) &&
					is_separator(row->render[i+klen])) {
						//if passed, use memset() to highlight the whole keyword at once.
						memset(&row->hl[i], kw2 ? HL_KEYWORD2:HL_KEYWORD1,klen);
						i += klen;
						break;
				}
			}
			if(keywords[j] != NULL){
				prev_sep = 0;
				continue;
			}
		}

		prev_sep = is_separator(c);
		i++;
	}

	int changed = (row->hl_open_comment != in_comment);
  	row->hl_open_comment = in_comment;
  	if (changed && row->idx + 1 < E.numrows)
    	editorUpdateSyntax(&E.row[row->idx + 1]);
}

int editorSyntaxToColor(int hl) {
	switch(hl) {
		case HL_COMMENT:
    	case HL_MLCOMMENT: return 36;
		case HL_KEYWORD1: return 33; //yellow
		case HL_KEYWORD2: return 32; //green
		case HL_STRING: return 35; //returning foreground magenta for Strings
		case HL_NUMBER: return 31; //Returning foreground red for numbers 
		case HL_MATCH: return 34; //return foreground blue 
		default: return 37; //returning foreground white for anything else
	}
}
void editorSelectSyntaxHighlight() {
	E.syntax = NULL; //if nothing matches or no filenmae, no filetype 
	if (E.filename == NULL) return;
	/*returns a pointer to last occurrence of a character in a string, used to get extension or filetype*/
	char *ext = strrchr(E.filename, '.'); //comes from string.h

	/*Looping through each editorSyntax struct in HLDB array, for each one of those we loop through each pattern
	in its filematch array. If pattern starts with a . then it's a file extension pattern and we use strcmp() to see
	if the filename end with that extension. If it’s not a file extension pattern, then we just check to see 
	if the pattern exists anywhere in the filename, using strstr(). If the filename matched according to those 
	rules, then we set E.syntax to the current editorSyntax struct, and return. */
	for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
		struct editorSyntax *s = &HLDB[j];
		unsigned int i = 0;
		while (s->filematch[i]) {
		int is_ext = (s->filematch[i][0] == '.');
		/*strcmp() returns 0 if two given strings are equal*/
		if ((is_ext && ext && !strcmp(ext, s->filematch[i])) || //also comes from string.
			(!is_ext && strstr(E.filename, s->filematch[i]))) {
			E.syntax = s;
			int filerow;
			//In case we create new file and save to disk, need to highlight after being saved
			for (filerow = 0; filerow < E.numrows; filerow++) {
			editorUpdateSyntax(&E.row[filerow]);
			}
			return;
		}
		i++;
		}
	}
}

/*** row operations ***/
int editorRowCxtoRx(erow *row, int cx){
	/*This function converts a chars index into a render index, We'll loop through all the characters to the left of cx
	and figure out how many spaces each tab takes up*/
	int rx =0;
	int j;
	for(j=0; j<cx;j++){
		//Is the current character to the left of cx a tab?
		if(row->chars[j] == '\t') {
			/*For each character, if it's a tab, we use rx %KILO_TAB_STOP to find out how many columns we are to the right of the
			last tab stop, and then subtract that from KILO_TAB_STOP -1 to find out how many columns we are to the left of the next 
			tab stop. We add that amount to rx to get just to the left of the next tab stop, and then the unconditional rx++ statement gets 
			us right on the next tabstop.*/
			rx += (KILO_TAB_STOP-1) - (rx % KILO_TAB_STOP);
		}
		rx++;
	}
	return rx;
}
int editorRowRxToCx(erow *row, int rx){
	//curr position
	int cur_rx = 0;

	/*To convert an rx into a cx, we do pretty muc hthe same thing when converting the other way:
	loop through the chars string, calc the current rx value(cur_rx) as we go, but don't stop when
	hutting a particular cx value, and returning cur_rx, we want to stop when cur_rx hits the given 
	rx value and return cx*/
	int cx; 
	for(cx = 0; cx<row->size;cx++){
		if(row->chars[cx] == '\t')
			cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
		cur_rx++;

		if(cur_rx > rx) return cx;
	}
	//Return statemetn just in case the caLLER provided an rx that's out of range, which shouldn't happen.
	return cx;
}
void editorUpdateRow(erow *row) {
	//whole points of the function, we use chars string of an erow to fill in the contents of the render string
	//We will copy each character from chars to render.
	int tabs = 0;
	int j;
	/*loop through the chars of the row and count the tabs in order to know how much meory to allocate for render
	the max # of chars needed for each tab is 8, row->size already counts 1 for each tab, so multiply the # of tabs by 7
	and add that to row->size to get max amount of mem we'll need for the rendered row*/
	for (j = 0; j < row->size; j++)
		if (row->chars[j] == '\t') tabs++;
	free(row->render);
	row->render = malloc(row->size + tabs*(KILO_TAB_STOP-1) + 1);
	int idx = 0;
	/*after allocating memory, we check whether the current character is a tab, if it is, we append one space(because each tab must 
	advance the cursor forward at least one column) and then append spaces until we get to a tab stop. which is a column that is divisible by 8*/
	for (j = 0; j < row->size; j++) {
		if (row->chars[j] == '\t') {
		row->render[idx++] = ' ';
		while (idx % KILO_TAB_STOP != 0) row->render[idx++] = ' ';
		} else {
		row->render[idx++] = row->chars[j];
		}
	}
	row->render[idx] = '\0';
	row->rsize = idx;

	//For syntax highlighting
	editorUpdateSyntax(row);
	/*This fun already has job of updating the render arr when the text of the row changes
	so thats where we want to update the hl array, so after updating render we call upsynt() at end*/

}

void editorInsertRow(int at, char *s, size_t len) {
	if (at < 0 || at > E.numrows) return;
	E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
	memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));
	
	for (int j = at + 1; j <= E.numrows; j++) E.row[j].idx++;

	E.row[at].idx = at;

	E.row[at].size = len;
	E.row[at].chars = malloc(len + 1);
	memcpy(E.row[at].chars, s, len);
	
	E.row[at].chars[len] = '\0';
	E.row[at].rsize = 0;
	E.row[at].render = NULL;
	//hl is an array of unsigned char calues, (0-255) Each val will correspond to a char in render.
	//telling you whether that char is part of a string, comment, or number.
	E.row[at].hl = NULL;
	E.row[at].hl_open_comment = 0;
	editorUpdateRow(&E.row[at]);
	
	E.numrows++;
	E.dirty++;
}
void editorFreeRow(erow *row) {
	free(row->render);
	free(row->chars);
	free(row->hl);
}
void editorDelRow(int at) {
	/*Looks like editorRowDelChar() because in both cases we are deleting a single element from 
	 from an array of elements by its index
	 Frist we validate the at index. Then we free the memory owned by the row using editorFreeRow()
	 we then use memove() to overwrite the deleted row struct with the rest of the rows that come after it, and we decrement numrows
	 finally increment E.dirty*/
	if (at < 0 || at >= E.numrows) return;
	editorFreeRow(&E.row[at]);
	memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
	for (int j = at; j < E.numrows - 1; j++) E.row[j].idx--;
	E.numrows--;
	E.dirty++;
}
void editorRowInsertChar(erow *row, int at, int c){
	//Validate at, (insert char position)
	if(at < 0 || at > row->size) at = row->size;
	/*Allocate one more byte for the chars of the erow (add 2 because we also make room for the null byte)
	and use memmove() to make room for the new character*/
	row->chars = realloc(row->chars, row->size +2);
	/*Comes from string.h, similar to memcpy() but its safe to use when the source and destination
	arrays overlap */
	memmove(&row->chars[at+1],&row->chars[at], row->size -at +1);
	//increment size of the chars array
	row->size++;
	row->chars[at] = c;
	editorUpdateRow(row);
	E.dirty++;
}
void editorRowAppendString(erow *row, char *s, size_t len){
	row->chars = realloc(row->chars, row->size + len +1);
	/*interesting statement, it's been a while since I've coded in C (real life is busy) so lets walk through this statement
	memcpy(dest, src, len) copies len bytes from src to dest
	row->chars[row->size] is the location in the character array chars (inside a row pointer that points to the pointer where the struct is (i think))
	where we start copying new data. the & gets the address of that spot
	s is a pointer to the data you want to copy
	len is the bytes/chars to copy*/
	memcpy(&row->chars[row->size], s , len);
	row->size += len;
	row->chars[row->size] = '\0';
	E.dirty++;
	/*The row’s new size is row->size + len + 1 (including the null byte), so first we allocate that much memory for row->chars. 
	Then we simply memcpy() the given string to the end of the contents of row->chars. We update row->size, call editorUpdateRow() 
	as usual, and increment E.dirty as usual.*/
}
void editorRowDeleteChar(erow *row, int at){
	if(at < 0 || at >= row->size) return;
	memmove(&row -> chars[at], &row ->chars[at +1], row->size -at);
	row->size--;
	editorUpdateRow(row);
	E.dirty++;
}

/*** editor operations ***/
void editorInsertChar(int c){
	/*if true, means the cursor is on the tilde line after the end of the file
	so we need to append a new row to the file before inserting a character there*/
	if(E.cy == E.numrows){
		editorInsertRow(E.numrows, "" ,0);
	}
	editorRowInsertChar(&E.row[E.cy], E.cx, c);
	//move the cursor forward
	E.cx++;
}
void editorInsertNewline(){
	if(E.cx == 0){
		editorInsertRow(E.cy, "", 0);
	} else {
		erow *row = &E.row[E.cy];
		editorInsertRow(E.cy +1, &row->chars[E.cx], row->size - E.cx);
		row = &E.row[E.cy];
		row->size = E.cx;
		row->chars[row->size] = '\0';
		editorUpdateRow(row);
	}
	E.cy++;
	E.cx=0;
}
void editorDeleteChar(){
	//Cursor past EOF, nothing to delete.
	if(E.cy == E.numrows) return;

	//If cursory is at begin of first line, do nothing.
	
	if(E.cx == 0 && E.cy == 0) return;
	/*Grab the erow the cursor is on, if there is a character to the left of the cursor we delete it*/
	erow *row = &E.row[E.cy];
	if(E.cx > 0){
		editorRowDeleteChar(row, E.cx -1);
		E.cx--;
	} else {
		/*If we find that E.cx == 0, we call editorRowAppendString() as we planned, row points to the row we are deleting
		so we append row->chars to the previous row, and then delete the row that E.cy is on. Set E.cx to the end of the contents
		of the previous row before appending to that row. Cursor ends up at the point where the two lines joined */
		E.cx = E.row[E.cy-1].size;
		editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
		editorDelRow(E.cy);
		E.cy--;
	}
}

/*** file i/o ***/
//memcpy(void *dest, const void *src, ssize_t (SIZE))
//mallloc comes from stydlib.h, ssize_ comes from above libraries, sys/types.h
void editorOpen(char *filename) {

	//This is practically the same as the old version, we just get the line and linelen values from the getline() now
	//editorOpen() takes filename and opens the file for reading using fopen(). allowing user to choose a file to open bu checking
	//if they passed a filename as a commande line argument. 
	free(E.filename);
	/*This comes from string.h, making a copy of the given string, allocating the req memory and assuming you will free() it*/
	E.filename = strdup(filename);
	
	editorSelectSyntaxHighlight();

	FILE *fp = fopen(filename, "r");
	if (!fp) die("fopen");
	char *line = NULL;
	size_t linecap = 0;
	ssize_t linelen;
	linelen = getline(&line, &linecap, fp);
	//This little line will now read the entire file we open which is pretty coolio dudeo
	//It works greate because getline() return -1 when it gets to the end of the file and has no more lines to read.
	//getline does a lot of work behind the scenes with mem allocation. very nice C 
	while((linelen = getline(&line, &linecap, fp)) != -1) {
		while (linelen > 0 && (line[linelen - 1] == '\n' ||
							line[linelen - 1] == '\r'))
		linelen--;
		editorInsertRow(E.numrows, line, linelen);
	}
	free(line);
	fclose(fp);
	E.dirty = 0;
}
char *editorRowsToString(int *buflen) {
	int totlen = 0;
	int j;
	/*Firstly, add up the lengths of eac hrow fo text, adding 1 to each one for the newline character we'll
		add up the end of each line. We save total len into buflen to tell the caller how long the string is*/
	for (j = 0; j < E.numrows; j++)
		totlen += E.row[j].size + 1;
	*buflen = totlen;
	//allocating memory
	char *buf = malloc(totlen);
	char *p = buf;
	/*Looping through the rows, and memcpy() the contents of each row to the end of the buffer, appending a newline character 
	after each row.*/
	for (j = 0; j < E.numrows; j++) {
		memcpy(p, E.row[j].chars, E.row[j].size);
		p += E.row[j].size;
		*p = '\n';
		p++;
	}
	//return buf, expecting the caller to free() the memory.
	return buf;
}
void editorSave() {
		/*IF new file, E.filename will be NULL and we won't know where to save the file so we just return*/
	if (E.filename == NULL) {
		E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
		if (E.filename == NULL) {
			editorSetStatusMessage("Save aborted");
			return;
		}
		editorSelectSyntaxHighlight();
	}
	
	/*Calling editorRowsToString(), and write() the string to the path in E.filename. We tell
	open() we want to create a new file if it doesn't already exist (O_CREAT), and we want to open 
	it for reading and writing (O_RDWR). we used the O_CREAT flag, we have to pass xtra argument containing
	the mode (permissions) the new file should have. 066 is standard perms you usually want for these files
	owner = read + write, other = read*/
	int len;
	char *buf = editorRowsToString(&len);

	/*Normal way to overwrite a file is to pass the O_TRUNC flag to open(), which truncates the file completely,
	making it an empty file, before writing the new dat into it. By truncating ourselves to the same length as
	 the data we are planning to write into it, we are making the whole overwritting op a little bit safer in case the 
	 ftruncate() calls succeeds but the write() call fails. In which the file would still contain most of the data before
	 but the file was truncated completely by the open() call and then write() failed you'd end up with all of your 
	 lost data.*/
	 
	 /*More advanced editors will write to a new, temporary file, 
	 and then rename that file to the actual file the user wants to overwrite, 
	 and they’ll carefully check for errors through the whole process.*/
	int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
	if(fd != -1){
		/*Both open and ftruncate() return -1 on error. We expect write() to return the # of bytes
		we told it to write. Whether an error occurred or not, we ensure that the file is closed and the mem that
		buf points to is freed*/
		if(ftruncate(fd,len) != -1) {
			if(write(fd,buf,len) == len) {
				close(fd);
				free(buf);
				E.dirty = 0;
				editorSetStatusMessage("%d bytes written to disk", len);
				return;
			}
		}
		close(fd);
	}
	free(buf);
	editorSetStatusMessage("Can't save! I/O error: %s",strerror(errno));
}


/*** find ***/
void editorFindCallback(char *query, int key) {
	/*looping through all rows of the file, use strstr() to check if query is a substring of the currnt row.
	returning null if no match. otherwise return a pointer to matching substring. To convert that into
	an index that we can set E.cx to, substract the row->render pointer from the match pointer, since match
	is a pointer into the row->render string.
	finally setting E.rowoff so we are scrolled to very bottom of file which causes editorScroll() to scroll upwards 
	at the next screen refresh so that the matching line will be at the very top of the screen (no looking for searched)*/

	/*Also, in the callback, we check if the user pressed enter or escape, in which case they are leaving search mode
	so we just return immediately instead of doing another search.*/
	static int last_match = -1;
	static int direction = 1;


	/*static var to know which line hl needs to be restore*/
	static int saved_hl_line;
	/*dynamic allocated array which points to NULL when nothing to restore. If there is something to restore
	we memcpy() it to the saved line's hl and then deallocate saved_hl and set it back to NULL*/

	/*Notice that hte malloc()'d memory is guared to be free()'d because when the user closes the search prompt 
	by pressing enter or escape, editorPrompt() calls our callback, giving a change for hl to be restore before 
	editorPrompt() finally returns, also impossible for saved_hl to get malloc()'d befroe its old value gets free()'d 
	because we always free() it at the top of hte funciton. And finally it's impossible fore hte user to edit the 
	file between saving and restoring the hl, so we safely use saved_hl_line as an index into E.row*/
	static char *saved_hl = NULL;
	if(saved_hl){
		memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
		free(saved_hl);
		saved_hl = NULL;
	}

	if (key == '\r' || key == '\x1b') {
		last_match = -1;
		direction = 1;
		return;
	} else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
		direction = 1;
	} else if (key == ARROW_LEFT || key == ARROW_UP) {
		direction = -1;
	} else {
		last_match = -1;
		direction = 1;
	}

	if(last_match == -1) direction = 1;
	/*current is the index of the current row we're searching, if there was a last match
	it starts on the line after (or before, if we're searching backwards). if wasn't a last
	matchm it starts at htet op of the file and searches in the forward direction to find the first 
	match.

	the if .. else  causes current to go from the end of the file back to the beginning of the 
	file or vice vers, to allow a search to "wrap around" the end of a fiule and continue from the top
	(or bottom)
	*/
	int current = last_match;
	int i;
	for (i = 0; i < E.numrows; i++) {
		current += direction;
		if(current == -1) current = E.numrows -1;
		else if(current == E.numrows) current = 0;
		erow *row = &E.row[current];
		char *match = strstr(row->render,query);
		if(match){
			last_match = current;
			E.cy= current;
			E.cx = editorRowRxToCx(row, match - row->render);
			E.rowoff = E.numrows;

			saved_hl_line = current;
			saved_hl = malloc(row->rsize);
			memcpy(saved_hl, row->hl, row->rsize);
			//match - row->render is the index into render of the match, so we use that as our index into hl
			memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
			break;
		}

 	}
}
void editorFind() {
	int saved_cx = E.cx;
	int saved_cy = E.cy;
	int saved_coloff = E.coloff;
	int saved_rowoff = E.rowoff;

	char *query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)",
                             editorFindCallback);
	
	if (query) {
    	free(query);
	} else {
		E.cx = saved_cx;
		E.cy = saved_cy;
		E.coloff = saved_coloff;
		E.rowoff = saved_rowoff;
	}
}
/*** APPEND BUFFER ***/
//This is really annoying c jargen and these methods are used to allocate enough memory to hold the new string.

struct abuf {
	char *b; //pointer to buffer in memory
	int len;
};
#define ABUF_INIT {NULL, 0} // ACTING as constructor from our abuf type

void abAppend(struct abuf *ab, const char *s, int len){
	//To append a new string s onto abuf. we need to use realloc
	char *new = realloc(ab -> b, ab ->len + len);//comes from string. gives us a block of memory to work with
												 //so we can append the new string onto the previous string.
	if(new == NULL) return ;

	//memcpy copies the string s after the end of the current data  in the buffer.
	//update the pointer and length of abuf to the new values. 
	// In hindsight, I'm very glad C gives us the ability to allocate memory 
	// to append new data onto strings (Quite cool)
	memcpy(&new[ab->len], s, len);
	ab->b = new;
	ab->len += len;
}
//Deconstructor that deallocate thes dynamic memory used by an abuf
void abFree(struct abuf *ab){
	free(ab->b);
}
/*** OUTPUT ***/
void editorDrawStatusBar(struct abuf *ab) {
	abAppend(ab, "\x1b[7m", 4);
	//Drawing status Bar
	/*To make this thang stand out, we're going to display with inverted colors.*/
	char status[80], rstatus[80]; //diplays up to 80 chars
	
	//Printing the data, i.e lines in file and name of file
	int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
    					E.filename ? E.filename : "[No Name]", E.numrows,
    					E.dirty ? "(modified)" : "");

	/*Showing the current line number and # of lines in the file
	E.cy contains the current line number but add 1 as it's 0 indexed*/
	  int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d",
    		E.syntax ? E.syntax->filetype : "no ft", E.cy + 1, E.numrows);
	//cut len if too big
	if(len > E.screencols) len = E.screencols;
	abAppend(ab, status, len);
	while (len < E.screencols) {
		if(E.screencols - len == rlen){
			abAppend(ab, rstatus, rlen);
			break;
		} else {
			abAppend(ab, " ", 1);
			len++;
		}
	}
	abAppend(ab, "\x1b[m", 3);
	abAppend(ab, "\r\n", 2);
}
void editorDrawMessageBar(struct abuf *ab){
	abAppend(ab, "\x1b[K",3);
	int msglen = strlen(E.statusmsg);
	if(msglen > E.screencols) msglen = E.screencols;
	if(msglen && time(NULL) - E.statusmsg_time < 5) {
		abAppend(ab, E.statusmsg,msglen);
	}
}
void editorScroll(){
	E.rx = 0;
	if(E.cy < E.numrows){
		E.rx = editorRowCxtoRx(&E.row[E.cy], E.cx);
	}

	//checks if the cursor is above the visible window, if so it will scroll up
	if(E.cy < E.rowoff){
		E.rowoff = E.cy;
	}

	//checking whether the cursor is at the bottom, if so scroll down.
	if(E.cy >= E.rowoff + E.screenrows){
		E.rowoff = E.cy - E.screenrows +1;
	}

	//Checking for horizontal scrolling.
	if (E.rx < E.coloff) {
		E.coloff = E.rx;
	}
	if (E.rx >= E.coloff + E.screencols) {
		E.coloff = E.rx - E.screencols + 1;
	}
}
void editorDrawRows(struct abuf *ab) {
	int y;
	for (y = 0; y < E.screenrows; y++) {
		int filerow = y + E.rowoff;
		if (filerow >= E.numrows) {
		if (E.numrows == 0 && y == E.screenrows / 3) {
			char welcome[80];
			int welcomelen = snprintf(welcome, sizeof(welcome),
			"DockerKilo -- version %s", DOCKERKILO_VERSION);
			if (welcomelen > E.screencols) welcomelen = E.screencols;
			int padding = (E.screencols - welcomelen) / 2;
			if (padding) {
			abAppend(ab, "~", 1);
			padding--;
			}
			while (padding--) abAppend(ab, " ", 1);
			abAppend(ab, welcome, welcomelen);
		} else {
			abAppend(ab, "~", 1);
		}
		} else {
			int len = E.row[filerow].rsize - E.coloff;
			if (len < 0) len = 0;
			if (len > E.screencols) len = E.screencols;
			/*Grabbing the current rendered character and determining whether it's a color
			Now what's interesting is that the VT100 User Guide (A video terminal) in which
			it defines how to communicate with the vt100 over a serial connection using escape sequences
			such as moving the cursor, clearing the screen and changing text attributes. However in this magic
			guide it does not document the altering of text colour, hence we refer to the wiki article 
			on ANSI escape codes. containing a large table of all the diff argument codes you can use with the
			m command on various terminals.*/
			char *c = &E.row[filerow].render[E.coloff];
			
			unsigned char *hl = &E.row[filerow].hl[E.coloff]; //pointer, hl to the slice of the hl array correspoinding to the slice of render printing
			
			//curr color keeps track of the current colour and keeps track of a change (helping performance)
			int current_color = -1;
			int j;
				//for each char, if it's an HL_NORMAL character
				/*Utilise <esc>[39m] to make sure we're using default text color before printing it.
				If it's not HL_NOMRAL, use snprintf() to write the escape sequence into a buffer which we pass to abAppend() before 
				appending the character. After finished looping through all the chars and displaying them, print final escape sequence
				<esc>[39m to make sure the color is reset to default*/
			for (j = 0; j < len; j++) {
				//iscntrl() to check if curr char is control character
			    if (iscntrl(c[j])) {
					//translate to printable char
					char sym = (c[j] <= 26) ? '@' + c[j] : '?';
					abAppend(ab, "\x1b[7m", 4);
					abAppend(ab, &sym, 1);
					abAppend(ab, "\x1b[m", 3);
					 if (current_color != -1) {
						//<esc>[m turns off all text formatting, printing escape sequence for current color afterwards
						char buf[16];
						int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
						abAppend(ab, buf, clen);
					}
				} else if (hl[j] == HL_NORMAL) {	
					if (current_color != -1) {
						abAppend(ab, "\x1b[39m", 5);
						current_color = -1;
					}
					abAppend(ab, &c[j], 1);
				} else {
					int color = editorSyntaxToColor(hl[j]);
					if (color != current_color) {
						current_color = color;
						char buf[16];
						int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
						abAppend(ab, buf, clen);
					}
					abAppend(ab, &c[j], 1);
				}
			}
			abAppend(ab, "\x1b[39m", 5);
			}
			abAppend(ab, "\x1b[K", 3);
			abAppend(ab, "\r\n", 2);
		}
}


void editorRefreshScreen() {
	editorScroll();
	struct abuf ab = ABUF_INIT;
	//Appends the escape sequence to move the cursor to the top-left corner ([H means "move to row 1, column 1").
	//possible cursor might be displayed in the middle of the screen somewhere for a split second.
	abAppend(&ab, "\x1b[?25l", 6);
	abAppend(&ab, "\x1b[H", 3);
	editorDrawRows(&ab);
	editorDrawStatusBar(&ab);
	editorDrawMessageBar(&ab);
	char buf[32];
	snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 1,
												(E.rx - E.coloff) + 1);
	abAppend(&ab, buf, strlen(buf));
	abAppend(&ab, "\x1b[?25h", 6);
	write(STDOUT_FILENO, ab.b, ab.len);
	abFree(&ab);
}
void editorSetStatusMessage(const char *fmt, ...){
	/*Okay, lets walk through this*/
	/*va_list, va_start and va_end come from stdarg.h. vsnprintf() comes from stdio.h. time() comes from time.h
	In main() we set our initial message which is just a helpful how-to quit. vsnprintf() helps us make our own printf() style function
	we store the resulting string in E.statusmsg, and set E.statusmsg_time to the curr time, which can be gotten by passing NULL to time()
	(by convention returning the # of seconds that have passed since midnight, Jan 1, 1970 as an int)*/

	/*The ... arg makes editorSetStatusMessage a variadic function (variadic function is a function of indefinite arity, i.e., 
	one which accepts a variable number of arguments. Support for variadic functions differs widely among programming languages.)
	since we can pass in any # of arguements, c's way of dealing with these args is by having you call va_start() and va_end() on a value
	of  type va_list. The last argument before the ... (in this case fmt) must be passed to va_start(), so that the address of the next
	arguments is known. Then, between the va)start() and va_end() calls, you would call va_arg)_ and pass it the type of the next argument
	(which you usually get from the given format string) and it would return the value of that argument. we pass fmt and ap to vsnprintf() 
	and it takes care of reading the format string and calling va_arg() to get each argument. */
	va_list ap;
	va_start(ap, fmt);
	vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
	va_end(ap);
	E.statusmsg_time = time(NULL);
}

/*** INPUT ***/
char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
	size_t bufsize = 128;
	char *buf = malloc(bufsize);

	size_t buflen = 0;
	buf[0] = '\0';

	/*bad practice don't care*/
	while(1){
		editorSetStatusMessage(prompt, buf);
		editorRefreshScreen();

		int c = editorReadKey();
		if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
			if (buflen != 0) buf[--buflen] = '\0';
		} else if (c == '\x1b') {
			editorSetStatusMessage("");
			if(callback) callback(buf,c);
			free(buf);
			return NULL;
		}else if(c == '\r'){
			if(buflen != 0){
				editorSetStatusMessage("");
				if(callback) callback(buf,c);
				return buf;
			}
		} else if(!iscntrl(c) && c < 128){
			if(buflen == bufsize - 1){
				bufsize *= 2;
				buf = realloc(buf, bufsize);
			}
			buf[buflen++] = c;
			buf[buflen] = '\0';
		}

		if(callback) callback(buf,c);
	}
}
void editorMoveCursor(int key) {
	erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
	switch (key) {
		case ARROW_LEFT:
		if (E.cx != 0) {
			E.cx--;
		}else if(E.cy > 0){ //Letting user press left arrow at beginnning of line to move to the end of previous line 
			E.cy--;
			E.cx = E.row[E.cy].size;
		}
		break;
		case ARROW_RIGHT:
		if (row && E.cx < row->size) {
			E.cx++;
		} else if(row && E.cx == row -> size) {//opposite of above comment
			E.cy++;
			E.cx = 0;
		}
		break;
		case ARROW_UP:
		if (E.cy != 0) {
			E.cy--;
		}
		break;
		case ARROW_DOWN:
		if (E.cy < E.numrows) {
			E.cy++;
		}
		break;
	}
	//Correcting E.cx if it ends up past the end of the line its on.
	row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
	int rowlen = row ? row->size : 0;
	if (E.cx > rowlen) {
	E.cx = rowlen;
  }
}
void editorProcessKeypress() {
	static int quit_times = KILO_QUIT_TIMES;
  	int c = editorReadKey();
  	switch (c) {
		case '\r':
			editorInsertNewline();
			/* TODO */
			break;
    	case CTRL_KEY('q'):
		//Clearing the screen, reposition the cursor
		if(E.dirty && KILO_QUIT_TIMES > 0){
			editorSetStatusMessage("WARNING!!! File has unsaved changes. "
				"Press CTRL-Q %d more times to quit.", quit_times);
				quit_times--;
				return;
		}
	  	write(STDOUT_FILENO, "\x1b[2J", 4);
      	write(STDOUT_FILENO, "\x1b[H", 3);
	    exit(0);
      	break;

		case CTRL_KEY('s'):
			//Saving file.
			editorSave();
			break;

		case HOME_KEY:
			E.cx=0;
			break;
		case END_KEY://moving to the end of current row
			if(E.cy < E.numrows)
				E.cx = E.row[E.cy].size;
			break;

		case CTRL_KEY('f'):
			editorFind();
			break;
		case BACKSPACE:
    	case CTRL_KEY('h'):
    	case DEL_KEY:
      		if(c == DEL_KEY) editorMoveCursor(ARROW_RIGHT);
			editorDeleteChar();
      		break;
		
			case PAGE_DOWN:
		case PAGE_UP:
		{
			//Scrolling up and down the page 
			if(c == PAGE_UP){
				E.cy = E.rowoff;
			}else if(c == PAGE_DOWN){
				E.cy = E.rowoff + E.screenrows -1;
				if(E.cy > E.numrows) E.cy = E.numrows;
			}
			int times = E.screenrows;
			while(times--){
				editorMoveCursor(c == PAGE_UP? ARROW_UP:ARROW_DOWN);
			}
			break;
		}	
    	case ARROW_UP:
    	case ARROW_DOWN:
    	case ARROW_LEFT:
    	case ARROW_RIGHT:
      		editorMoveCursor(c);
      		break;
		
		case CTRL_KEY('l'):
    	case '\x1b':
      		break;
		
			default:
			editorInsertChar(c);
			break;
	}
	quit_times = KILO_QUIT_TIMES;
}

/*** INIT ***/
void initEditor(){
	E.cx = 0;
	E.cy = 0;
	E.rx = 0;
	E.rowoff = 0;
	E.coloff = 0;
	E.numrows = 0;
	E.row = NULL;
	E.filename = NULL;
	E.statusmsg[0] = '\0'; //Initialise an empty string
	E.statusmsg_time = 0;
	E.dirty = 0;
	if(getWindowSize(&E.screenrows, &E.screencols) == -1) die("Get Windows Size");
	/*Decrement E.screenrows so that editorDrawRows() doesn't try to draw a line of text at bottom of screen.*/
	E.screenrows -= 2;
	E.syntax = NULL;
}
int main(int argc, char *argv[]){
	enableRawMode();
	initEditor();//Initializing the editor to get the terminal screenrows/cols the EASY way. (why make this difficult)
	//asking to read 1 byte from the standard input into the var c, returns 0 when no more bytes left to read
	//Terminal will always start in cooked mode(keyboard input is only sent to your program when the user presses enter)
	//We want raw mode where the user keypresses are inputting as they are typed.

	if(argc >= 2){
		editorOpen(argv[1]);
	}

	editorSetStatusMessage(
    "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

	while(1){
		editorProcessKeypress();
		editorRefreshScreen();
	}

	return 0;
}

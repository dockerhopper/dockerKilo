# DockerKilo - Text Editor

![DockerKilo Demo](demo.gif)

## Overview
DockerKilo is a fork of the Kilo text editor with multi-buffer support, improved syntax highlighting, and advanced file management capabilities. DockerKilo maintains Kilo's lightweight philosophy while adding some interesting features

## Key Features

### 1. Multi-Buffer Management
DockerKilo introduces a revolutionary buffer system allowing simultaneous editing of multiple files:

- **10 simultaneous buffers** with `Ctrl-B` to create new buffers
- **Cyclical navigation** with `Ctrl-N` (next) and `Ctrl-P` (previous)
- **Intelligent buffer closing** with `Ctrl-W` including save prompts
- **Persistent cursor positions** maintained per buffer
- **Visual buffer indicators** in status bar: `[current/total]`

```c
struct editorConfig {
    struct editorBuffer buffers[MAX_BUFFERS];
    int current_buffer;
    int num_buffers;
};
```
# Buffer Implementation 
Buffer, implementation that keeps local constants with regards to each individual buffer. It allows for multiple files to be open at the same time
while maintaining read/write capabilities over all of them
```c
#define MAX_BUFFERS 10  // Maximum number of open buffers

struct editorBuffer {
    int cx, cy;         // Cursor position
    int rx;             // Render index
    int rowoff;         // Row offset for scrolling
    int coloff;         // Column offset for scrolling
    int numrows;        // Number of rows
    erow *row;          // Row data
    int dirty;          // Unsaved changes flag
    char *filename;     // Current filename
    struct editorSyntax *syntax;  // Syntax highlighting rules
};
```
# Key Features
#### 1. Multi-Buffer Management
DockerKilo introduces a revolutionary buffer system allowing simultaneous editing of multiple files:

```c
struct editorConfig {
    struct editorBuffer buffers[MAX_BUFFERS];
    int current_buffer;
    int num_buffers;
};
```
10 simultaneous buffers with Ctrl-B to create new buffers

Cyclical navigation with Ctrl-N (next) and Ctrl-P (previous)

Intelligent buffer closing with Ctrl-W including save prompts

Persistent cursor positions maintained per buffer

Visual buffer indicators in status bar: [current/total]

#### 2. Enhanced Syntax Highlighting
Advanced parsing engine with language detection:

```c
struct editorSyntax {
    char *filetype;
    char **filematch;       // File extension patterns
    char **keywords;        // Language keywords
    char *singleline_comment_start;
    char *multiline_comment_start;
    char *multiline_comment_end;
    int flags;              // Highlighting options
};
```
C/C++ support with nested comment handling

Multi-line string detection

Number recognition with decimal point handling

Language auto-detection based on file extension

Customizable color schemes via editorSyntaxToColor()

#### 3. Advanced Rendering System
Efficient screen rendering with optimized display algorithms:

```c
void editorUpdateRow(struct editorBuffer *buf, erow *row) {
    // Convert raw characters to rendered text
    int tabs = 0;
    for (int j = 0; j < row->size; j++)
        if (row->chars[j] == '\t') tabs++;
    
    row->render = malloc(row->size + tabs*(KILO_TAB_STOP-1) + 1);
    
    // Tab expansion logic
    int idx = 0;
    for (int j = 0; j < row->size; j++) {
        if (row->chars[j] == '\t') {
            row->render[idx++] = ' ';
            while (idx % KILO_TAB_STOP != 0) row->render[idx++] = ' ';
        } else {
            row->render[idx++] = row->chars[j];
        }
    }
}
```
Tab expansion with configurable tab stops

Line wrapping with horizontal scrolling

Efficient screen updates using append buffers

Smart cursor positioning during editing operations

#### 4. Search and Navigation
Powerful text search with incremental highlighting:

```c
void editorFindCallback(char *query, int key) {
    static int last_match = -1;
    static int direction = 1;
    
    // Arrow key handling for search direction
    if (key == ARROW_RIGHT || key == ARROW_DOWN) direction = 1;
    else if (key == ARROW_LEFT || key == ARROW_UP) direction = -1;
    
    // Wrap-around search implementation
    int current = last_match;
    for (int i = 0; i < buf->numrows; i++) {
        current += direction;
        if(current == -1) current = buf->numrows -1;
        else if(current == buf->numrows) current = 0;
        
        // String matching with highlighting
        erow *row = &buf->row[current];
        char *match = strstr(row->render, query);
        if(match) {
            // Highlight matching text
            memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
            last_match = current;
            // Position cursor at match
            buf->cy = current;
            buf->cx = editorRowRxToCx(row, match - row->render);
        }
    }
}
```
Incremental search with Ctrl-F

Match highlighting in blue

Wrap-around navigation at file boundaries

Directional searching with arrow keys

Installation
Prerequisites
GCC compiler

Linux/Unix-like system

Standard C library

Compilation
bash
gcc -o dockerKilo dockerKilo.c
sudo mv dockerKilo /usr/local/bin/
Docker Integration
dockerfile
FROM alpine:latest

RUN apk add build-base
COPY dockerKilo.c /app/dockerKilo.c
RUN cd /app && gcc -o dockerKilo dockerKilo.c

ENTRYPOINT ["/app/dockerKilo"]
Usage
Basic Operations
bash
# Open single file
./kilo.c filename.c
Or ./kilo.c to open a blank file for your leisure.


# Buffer Management
Command	Function
Ctrl-B	Create new buffer
Ctrl-N	Next buffer
Ctrl-P	Previous buffer
Ctrl-W	Close current buffer
Ctrl-S	Save buffer
Ctrl-Q	Quit (with confirmation)
Ctrl-F	Search in file
Navigation Shortcuts
Key	Action
Arrow Keys	Move cursor
Page Up/Dn	Scroll full pages
Home	Jump to line start
End	Jump to line end
Ctrl+Arrow	Word-based navigation
Customization
Syntax Highlighting
Extend language support by modifying HLDB:

```c
struct editorSyntax HLDB[] = {
    {
        "python",
        { ".py", NULL },
        { "def", "class", "import", "from", "True", "False", "None", 
          "if", "elif", "else", "for", "while", "break", "continue", 
          "return", "in", "is", "not", "and", "or", "as", "pass", 
          "try", "except", "finally", "raise", "with", "yield", 
          "int|", "str|", "float|", "bool|", "list|", "dict|", "tuple|", NULL },
        "#", "\"\"\"", "\"\"\"",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    },
    // Add additional languages here
};
```
# Configuration Constants
```c
#define MAX_BUFFERS 10       // Maximum open buffers
#define KILO_TAB_STOP 4      // Tab size (spaces)
#define KILO_QUIT_TIMES 3    // Ctrl-Q presses required to quit dirty buffer
#define HL_HIGHLIGHT_NUMBERS (1<<0)  // Enable number highlighting
```
# Architecture
Core Components
Buffer Management System

- Circular buffer array for efficient switching
- Per-buffer state persistence
- Automatic resource cleanup
- Rendering Pipeline
- Raw character processing
- Tab expansion and whitespace handling
- Syntax-aware color application
- Syntax Highlighting Engine
- State machine for context detection
- Nested structure handling
- Language-specific rule sets
- Input Processing
- Raw terminal input handling
- Escape sequence decoding
- Custom key binding system
- Append Buffers: Minimize write operations
- Efficient Searching: Boyer-Moore inspired algorithm


# Future Roadmap
- Split window support

- Copy/paste functionality

- Config file support

- Plugin system

- Terminal color scheme adaptation

- ANSI escape sequence rendering

Kilo was written by Salvatore Sanfilippo aka antirez and is released under the BSD 2 clause license.

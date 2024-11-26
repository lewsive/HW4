#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "literal_table.h"
#include "utilities.h"

//literal table to keep track of all the constants/literals in the program

// const table entries
typedef struct literal_table_entry_s {
    struct literal_table_entry_s *next;
    const char *text;
    word_type value;
    unsigned int offset;
} literal_table_entry_t;


static literal_table_entry_t *first;
static literal_table_entry_t *last;
static unsigned int next_word_offset;
static bool iterating;
static literal_table_entry_t *iteration_next;

//invariant check
static void literal_table_okay()
{
    bool emp = literal_table_empty();
    assert(emp == (next_word_offset == 0));
    assert(emp == (first == NULL));
    assert(emp == (last == NULL));
}

//returns the amount of space used by the literal table
unsigned int literal_table_size()
{
    return next_word_offset;
}

//check if table is empty
bool literal_table_empty()
{
    return next_word_offset == 0;
}

//check if table is full
bool literal_table_full()
{
    return false;
}

//initialize literal table
void literal_table_initialize(void) {
    first = NULL;
    last = NULL;
    next_word_offset = 0;
    literal_table_okay();
    iterating = false;
    iteration_next = NULL;
    literal_table_okay();
}

//finds the offset of sought if possible
int literal_table_find_offset(const char *sought, word_type value)
{
    literal_table_okay();
    literal_table_entry_t *entry = first;
    while (entry != NULL) {
	if (strcmp(entry->text, sought) == 0) {
	    return entry->offset;
	}
	entry = entry->next;
    }
    return -1;
}

//finds out if sought is in table
bool literal_table_present(const char *sought, word_type value) {
    literal_table_okay();
    return literal_table_find_offset(sought, value) >= 0;
}


//finds a specific word at an offset if possible
unsigned int literal_table_lookup(const char *val_string, word_type value) {
    int ret = literal_table_find_offset(val_string, value);
    if (ret >= 0) {
    	  return ret;
    }
    
    literal_table_entry_t *new_entry = (literal_table_entry_t *) malloc(sizeof(literal_table_entry_t));
    new_entry->text = val_string;
    new_entry->value = value;
    new_entry->next = NULL;
    ret = next_word_offset;
    new_entry->offset = next_word_offset++;
    
    if (new_entry == NULL) {
	    bail_with_error("error, not enough space in table");
    }
    if (first == NULL) {
    	first = new_entry;
    	last = new_entry;
    } else {
    	last->next = new_entry;
    	last = new_entry;
    }
    literal_table_okay();
    return ret;
}

//iterate through the table
void literal_table_start_iteration() {
    if (iterating) {
        bail_with_error("error, already iterating");
    }
    literal_table_okay();
    iterating = true;
    iteration_next = first;
}


//end table iteration
void literal_table_end_iteration() {
    iterating = false;
}

//checks for another float in the table
bool literal_table_iteration_has_next() {
    literal_table_okay();
    bool ret = (iteration_next != NULL);
    if (!ret) iterating = false;
    return ret;
}

//returns the next float in the table if possible
word_type literal_table_iteration_next() {
    assert(iteration_next != NULL);
    word_type ret = iteration_next->value;
    iteration_next = iteration_next->next;
    return ret;
}
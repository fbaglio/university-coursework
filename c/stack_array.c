#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>

typedef struct {
    int key;
    float elem;
} entry;

int H = 100; //If we want, we can make the array dynamic...


int get_int(char* text_to_print);
float get_float(char* text_to_print);
entry* get_entry();

int isEmpty(int n);
int push(entry** pila, int* n, entry* e);
entry* top(entry** pila, int n);
entry* pop(entry** pila, int* n);

void print_pila(entry** pila, int n);
void manage_pila(entry** pila, int* n);
int print_menu(int n);

void free_memory(entry** pila, int n);

int main(int argc, char *argv[]){
    // Implement a stack with a predefined capacity
    entry* pila[H];
    int n = -1; //Initially Empty
    manage_pila(pila, &n);

    free_memory(pila, n);
    return 0;
}


/****   I/O FUNCTIONS ****/
int get_int(char* text_to_print){
    printf("%s", text_to_print);
    int integer = INT_MAX;
    scanf("%d", &integer);
    printf("\n");
    return integer;
}
float get_float(char* text_to_print){
    printf("%s", text_to_print);
    float number = 0.0;
    scanf("%f", &number);
    printf("\n");
    return number;
}
entry* get_entry(){
    entry* e = malloc(sizeof(entry));
    e->key = get_int("Enter a key: ");
    e->elem = get_float("Enter the element: ");
    return e;
}


/* TO INTERACT WITH THE STACK */
int print_menu(int n){
    printf("\nYour Stack has %d elements\n", n+1);
    printf("What do you wanna do?\n");
    printf("1. Push\n2. Pop\n3. Top\n4. Print\n5. Exit\n");
    return get_int("Please, enter the number corresponding to your choice>");
}
void print_pila(entry** pila, int n){
    if (isEmpty(n)==1)
      printf("The stack is empty\n");
    else
      for(int i=n;i>=0; i--)
        printf("Key=%d e=%f\n", pila[i]->key,pila[i]->elem);
}
void manage_pila(entry** pila, int* n){
    int choice = print_menu(*n);
    while (choice!=5){
        switch(choice){
            case 1: {
                entry* e = get_entry();
                int done = push(pila, n, e);
                if (done == 1)
                  printf("Element added to the stack\n\n");
                break;
            }
            case 2:{
                entry* element = pop(pila, n);
                if (element==NULL)
                    printf("The stack is already empty\n");
                else{
                    printf("Removed: Key=%d, Element = %f\n", element->key, element->elem);
                    free(element);
                  }
                break;
            }
            case 3: {
                entry* element = top(pila, *n); //can only use linear search
                if (element==NULL)
                    printf("The stack is empty\n");
                else
                    printf("Key=%d, Element = %f\n", element->key, element->elem);
                break;
            }
            case 4:
                print_pila(pila, *n);
                break;
            default:
                printf("Invalid choice. Please enter a number between 1 and 5\n");
        }

        choice = print_menu(*n);
    }
    printf("Exit...\n");
}

/* STACK'S FUNCTIONS */
int isEmpty(int n){
  if (n<0)
    return 1;
  return 0;
};
int push(entry** pila, int* n, entry* e){
  if (*n>=H-1){
    printf("No more space in the stack\n"); //Here we shoulld double the size of the array
    return 0;
  }
  *n+=1;
  pila[*n]=e;
  return 1;
};
entry* top(entry** pila, int n){
  if (isEmpty(n)==1)
    return NULL;
  return pila[n];
};
entry* pop(entry** pila, int* n){
  if (isEmpty(*n)==1)
    return NULL;
  entry* p = pila[*n];
  *n -= 1;
  return p;
};

void free_memory(entry** pila, int n){
    while(n>=0){
        free(pila[n]);
        n--;
    }
}

/*
 * Phonebook management program implemented using a singly linked list.
 *
 * The program allows users to:
 * - add new contacts, preventing duplicate names;
 * - search for a contact's phone number by name;
 * - display all contacts living in a specified city.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STR_LEN 30
#define NUM_LEN 10+1 // 10 numbers + NULL terminator.

struct Contact {
    char name[STR_LEN];
    char number[NUM_LEN];
    char city[STR_LEN];
    struct Contact *next;
};

struct Contact *createContact (char *name, char *number, char *city) {
    struct Contact *new = malloc(sizeof(struct Contact));
    if (new == NULL) {
        printf("Memory allocation failed\n");
        return NULL;
    }
    strcpy(new->name, name);
    strcpy(new->number, number);
    strcpy(new->city, city);
    new->next = NULL;
    return new;
}

struct Contact *insertHead(struct Contact *head, char *name, char *number, char *city) {
    struct Contact *new = createContact(name, number, city);
    if (new == NULL) return head;
    new->next = head;
    head = new;
    return head;
}

struct Contact *findContact(struct Contact *head, char *name) {
    struct Contact *curr = head;
    while (curr != NULL) {
        if (strcmp(curr->name, name) == 0) return curr;
        curr = curr->next;
    }
    return NULL;
}

void displayByCity(struct Contact *head, char *city) {
    struct Contact *curr = head;
    int found = 0;
    while (curr != NULL) {
        if (strcmp(curr->city, city) == 0) {
            printf("%s\n", curr->name);
            found = 1;
        }
        curr = curr->next;
    }
    if (!found) printf("Contact not found\n");
}

void freePhoneBook(struct Contact *head) {
    struct Contact *curr = head;
    while (curr != NULL) {
        struct Contact *temp = curr;
        free(temp);
        curr = curr->next;
    }
}

int main(void) {
    struct Contact *head = NULL;
    char name[STR_LEN], number[NUM_LEN], city[STR_LEN];
    int choice;

    do {
        printf("\nMenu:\n");
        printf("1: Insert a new contact into the phonebook\n");
        printf("2: View the phone number of a contact by name\n");
        printf("3: View all contacts living in a specific city\n");
        printf("0: Exit the program\n");
        printf("Enter your choice: ");
        scanf("%d", &choice);

        switch(choice) {
            case 1:
                printf("Enter the name: ");
                scanf(" %[^\n]", name);
                printf("Enter the number: ");
                scanf("%s", number);
                printf("Enter the city: ");
                scanf(" %[^\n]", city);

                if (findContact(head, name) == NULL)
                    head = insertHead(head, name, number, city);
                else
                    printf("Contact already exists in the phonebook.\n");
                break;
            case 2:
                printf("Enter the contact name: ");
                scanf(" %[^\n]", name);

                struct Contact *contact = findContact(head, name);
                if (contact != NULL)
                    printf("Phone number of %s: %s\n", name, contact->number);
                else
                    printf("Contact not found.\n");
                break;
            case 3:
                printf("Enter the city: ");
                scanf(" %[^\n]", city);
                displayByCity(head, city);
                break;
            case 0:
                printf("Exiting the program...\n");
                freePhoneBook(head);
                break;
            default:
                printf("Invalid choice.\n");
                break;
        }
    } while (choice != 0);

    return 0;
}
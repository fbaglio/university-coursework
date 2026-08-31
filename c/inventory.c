/*
 * Store inventory management program implemented using a singly linked list.
 *
 * The program allows users to:
 * - add new products to the end of the inventory, preventing duplicate product codes;
 * - display the product with the highest stock;
 * - display all products below a specified price;
 * - exit the program.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Product {
    char name[30];
    int code;
    float price;
    int stock;
    struct Product *next;
};

struct Product *insertTail(struct Product *head, char *name, int code, float price, int stock) {
    struct Product *newProduct = malloc(sizeof(struct Product));

    if (newProduct == NULL) {
        printf("Memory allocation failed\n");
        return head;
    }

    strcpy(newProduct->name, name);
    newProduct->code = code;
    newProduct->price = price;
    newProduct->stock = stock;
    newProduct->next = NULL;

    if (head == NULL) return newProduct;

    struct Product *curr = head;

    while (curr->next != NULL) {
        curr = curr->next;
    }

    curr->next = newProduct;

    return head;
}

int findProductByCode(struct Product *head, const int code) {
    struct Product *curr = head;

    while (curr != NULL) {
        if (curr->code == code) return 1;
        curr = curr->next;
    }

    return 0;
}

void displayProductWithMaxStock(struct Product *head) {
    if (head == NULL) {
        printf("Inventory is empty.\n");
        return;
    }

    struct Product *curr = head;
    struct Product *maxStock = curr;

    while (curr != NULL) {
        if (curr->stock > maxStock->stock) {
            maxStock = curr;
        }

        curr = curr->next;
    }

    printf("Product with the highest stock:\n");
    printf("Name: %s\n", maxStock->name);
    printf("Code: %d\n", maxStock->code);
    printf("Price: %.2f\n", maxStock->price);
    printf("Stock: %d\n", maxStock->stock);
}

void displayProductsBelowPrice(struct Product *head, const float price) {
    if (head == NULL) {
        printf("Inventory is empty.\n");
        return;
    }

    struct Product *curr = head;
    int found = 0;

    printf("Products with price below %.2f:\n", price);

    while (curr != NULL) {
        if (curr->price < price) {
            printf("Name: %s\n", curr->name);
            printf("Code: %d\n", curr->code);
            printf("Price: %.2f\n", curr->price);
            printf("Stock: %d\n", curr->stock);

            found = 1;
        }

        curr = curr->next;
    }

    if (!found) {
        printf("No products found with price below %.2f\n", price);
    }
}

void freeInventory(struct Product *head) {
    struct Product *curr = head;

    while (curr != NULL) {
        struct Product *temp = curr;
        curr = curr->next;
        free(temp);
    }
}

int main(void) {
    struct Product *head = NULL;
    char name[30];
    int code, stock;
    float price;
    int choice;

    do {
        printf("\nMenu:\n");
        printf("1: Insert a new product into inventory\n");
        printf("2: Display the product with the highest stock\n");
        printf("3: Display all products with price below a specified value\n");
        printf("0: Exit program\n");
        printf("Enter your choice: ");
        scanf("%d", &choice);

        switch (choice) {
            case 1:
                printf("Enter product name: ");
                scanf(" %[^\n]", name);

                printf("Enter code: ");
                scanf("%d", &code);

                printf("Enter price: ");
                scanf("%f", &price);

                printf("Enter stock: ");
                scanf("%d", &stock);

                if (!findProductByCode(head, code)) {
                    head = insertTail(head, name, code, price, stock);
                } else {
                    printf("Code %d is already present.\n", code);
                }

                break;

            case 2:
                displayProductWithMaxStock(head);
                break;

            case 3:
                printf("Enter the maximum price: ");
                scanf("%f", &price);

                displayProductsBelowPrice(head, price);
                break;

            case 0:
                printf("Exiting program...\n");
                freeInventory(head);
                break;

            default:
                printf("Invalid choice.\n");
                break;
        }

    } while (choice != 0);

    return 0;
}
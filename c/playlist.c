/*
 * Music playlist management program implemented using a doubly linked list.
 *
 * The program allows users to:
 * - insert a new song at a specified position in the playlist;
 * - display all songs by a specified artist;
 * - calculate and display the total duration of songs by a specified artist;
 * - exit the program.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Song {
    char title[30];
    char artist[30];
    int len;
    struct Song *prev, *next;
};

struct Song *createSong(char *title, char *artist, int len) {
    struct Song *newSong = malloc(sizeof(struct Song));

    if (newSong == NULL) {
        printf("Memory allocation failed\n");
        return NULL;
    }

    strcpy(newSong->title, title);
    strcpy(newSong->artist, artist);
    newSong->len = len;
    newSong->prev = NULL;
    newSong->next = NULL;

    return newSong;
}

int getPlaylistLength(struct Song *head) {
    int length = 0;
    struct Song *curr = head;

    while (curr != NULL) {
        length++;
        curr = curr->next;
    }

    return length;
}

struct Song *insertAtPosition(struct Song *head, char *title, char *artist, int len, int position) {
    struct Song *newSong = createSong(title, artist, len);

    if (newSong == NULL) return head;

    if (head == NULL) return newSong;

    /* Insertion at the head. */
    if (position == 0) {
        newSong->next = head;
        head->prev = newSong;
        return newSong;
    }

    struct Song *curr = head;
    int pos = 0;

    while (curr->next != NULL && pos < position - 1) {
        curr = curr->next;
        pos++;
    }

    newSong->prev = curr;
    newSong->next = curr->next;

    if (curr->next != NULL) {
        curr->next->prev = newSong;
    }

    curr->next = newSong;

    return head;
}

void displaySongsByArtist(struct Song *head, char *artist) {
    struct Song *curr = head;
    int found = 0;

    while (curr != NULL) {
        if (strcmp(curr->artist, artist) == 0) {
            printf("%s\n", curr->title);
            found = 1;
        }

        curr = curr->next;
    }

    if (!found) {
        printf("No songs by %s found.\n", artist);
    }
}

int calculateDurationByArtist(struct Song *head, char *artist) {
    struct Song *curr = head;
    int duration = 0;

    while (curr != NULL) {
        if (strcmp(curr->artist, artist) == 0) {
            duration += curr->len;
        }

        curr = curr->next;
    }

    return duration;
}

void freePlaylist(struct Song *head) {
    struct Song *curr = head;

    while (curr != NULL) {
        struct Song *temp = curr;
        curr = curr->next;
        free(temp);
    }
}

int main(void) {
    struct Song *head = NULL;
    char title[30], artist[30];
    int choice;

    do {
        printf("\nMenu:\n");
        printf("1: Insert a new song into the playlist at a specified position\n");
        printf("2: Display all songs by a specified artist\n");
        printf("3: Calculate and display the total duration of songs by a specified artist\n");
        printf("0: Exit the program\n");
        printf("Enter your choice: ");
        scanf("%d", &choice);

        switch (choice) {
            case 1:
            {
                int position, len;

                printf("Enter the position to insert the song in the playlist: ");
                scanf("%d", &position);

                if (position < 0 || position > getPlaylistLength(head)) {
                    printf("Invalid position.\n");
                    break;
                }

                printf("Enter the title of the song to add: ");
                scanf(" %[^\n]", title);

                printf("Enter the artist name for the song %s: ", title);
                scanf(" %[^\n]", artist);

                printf("Enter the duration (in seconds) of the song %s: ", title);
                scanf("%d", &len);

                head = insertAtPosition(head, title, artist, len, position);
                break;
            }

            case 2:
                printf("Enter artist name: ");
                scanf(" %[^\n]", artist);

                displaySongsByArtist(head, artist);
                break;

            case 3:
                printf("Enter artist name: ");
                scanf(" %[^\n]", artist);

                printf(
                    "Total duration (in seconds) of songs by %s: %d\n",
                    artist,
                    calculateDurationByArtist(head, artist)
                );

                break;

            case 0:
                printf("Exiting the program...\n");
                freePlaylist(head);
                break;

            default:
                printf("Invalid choice.\n");
                break;
        }

    } while (choice != 0);

    return 0;
}
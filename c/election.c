/*
 * Election management program implemented using a singly linked list.
 *
 * The program allows users to:
 * - insert four votes into the list;
 * - check whether the elector with ID 3 has voted;
 * - count the votes received by each candidate and determine the winner.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Vote {
    int id;
    char candidate[30];
    int section;
    struct Vote *next;
};

struct Vote *insertTail(struct Vote *head, int id, char *candidate, int section) {
    struct Vote *newVote = malloc(sizeof(struct Vote));
    if (newVote == NULL) return head;

    newVote->id = id;
    strcpy(newVote->candidate, candidate);
    newVote->section = section;
    newVote->next = NULL;

    if (head == NULL) return newVote;

    struct Vote *curr = head;
    while (curr->next != NULL) {
        curr = curr->next;
    }

    curr->next = newVote;
    return head;
}

int findVoteById(struct Vote *head, int id) {
    struct Vote *curr = head;

    while (curr != NULL) {
        if (curr->id == id) return 1;
        curr = curr->next;
    }

    return 0;
}

void countVotesByCandidate(struct Vote *head) {
    struct Candidate {
        char name[30];
        int votes;
    };

    struct Candidate candidates[4];
    struct Vote *curr = head;
    int candidateCount = 0;

    while (curr != NULL) {
        int found = 0;

        for (size_t i = 0; i < candidateCount; i++) {
            if (strcmp(curr->candidate, candidates[i].name) == 0) {
                candidates[i].votes++;
                found = 1;
                break;
            }
        }

        if (!found) {
            strcpy(candidates[candidateCount].name, curr->candidate);
            candidates[candidateCount].votes = 1;
            candidateCount++;
        }

        curr = curr->next;
    }

    char winner[30];
    int maxVotes = candidates[0].votes;
    strcpy(winner, candidates[0].name);

    printf("\nElection results:\n");

    for (size_t i = 0; i < candidateCount; i++) {
        printf("Candidate: %s, %d votes\n",
               candidates[i].name,
               candidates[i].votes);

        if (candidates[i].votes > maxVotes) {
            maxVotes = candidates[i].votes;
            strcpy(winner, candidates[i].name);
        }
    }

    printf("Election winner: %s, %d votes\n", winner, maxVotes);
}

void freeVotes(struct Vote *head) {
    struct Vote *curr = head;

    while (curr != NULL) {
        struct Vote *temp = curr;
        curr = curr->next;
        free(temp);
    }
}

int main(void) {
    struct Vote *head = NULL;
    char candidate[30];
    int id, section;

    for (size_t i = 0; i < 4; i++) {
        printf("Insert ID: ");
        scanf("%d", &id);

        printf("Insert name: ");
        scanf(" %[^\n]", candidate);

        printf("Insert section: ");
        scanf("%d", &section);

        head = insertTail(head, id, candidate, section);
    }

    if (findVoteById(head, 3))
        printf("ID 3 elector has voted\n");
    else
        printf("ID 3 elector has not voted\n");

    countVotesByCandidate(head);

    freeVotes(head);

    return 0;
}
/*
 * Sorting algorithms implemented in C.
 *
 * The program implements:
 * - Selection Sort;
 * - Insertion Sort;
 * - Bubble Sort;
 * - Merge Sort.
 *
 * Each algorithm sorts an integer array in ascending order.
 */

#include <stdio.h>

/*
 * Swaps the values stored at two memory addresses.
 */
void swap(int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

/*
 * Prints the elements of an integer array.
 */
void printArray(int *a, int n) {
    printf("[");

    for (int i = 0; i < n; i++) {
        printf("%d", a[i]);

        if (i < n - 1) {
            printf(" ");
        }
    }

    printf("]\n");
}

/*
 * Selection Sort:
 * repeatedly finds the smallest element in the unsorted portion
 * of the array and places it at the beginning of that portion.
 */
void selectionSort(int *a, int n) {
    for (int i = 0; i < n - 1; i++) {
        int k = i;

        for (int j = i + 1; j < n; j++) {
            if (a[j] < a[k]) {
                k = j;
            }
        }

        swap(&a[i], &a[k]);
        printArray(a, n);
    }
}

/*
 * Insertion Sort:
 * builds the sorted portion of the array one element at a time
 * by inserting each element into its correct position.
 */
void insertionSort(int *a, int n) {
    for (int i = 0; i < n - 1; i++) {
        int e = a[i + 1];
        int j = i;

        while (j >= 0 && a[j] > e) {
            a[j + 1] = a[j];
            j--;
        }

        a[j + 1] = e;
        printArray(a, n);
    }
}

/*
 * Bubble Sort:
 * repeatedly compares adjacent elements and swaps them when
 * they are in the wrong order.
 */
void bubbleSort(int *a, int n) {
    for (int i = 0; i < n - 1; i++) {
        int swapped = 0;

        for (int j = 1; j < n - i; j++) {
            if (a[j - 1] > a[j]) {
                swap(&a[j - 1], &a[j]);
                swapped = 1;
            }
        }

        printArray(a, n);

        /*
         * If no elements were swapped during a pass,
         * the array is already sorted.
         */
        if (!swapped) break;
    }
}

/*
 * Merges two consecutive sorted portions of the array:
 * [i, m] and [m + 1, f].
 */
void merge(int *a, int i, int m, int f) {
    int temp[f - i + 1];

    int j = 0;
    int inf = i;
    int sup = m + 1;

    /*
     * Compare elements from both sorted portions
     * and copy the smaller one into the temporary array.
     */
    while (inf <= m && sup <= f) {
        if (a[inf] <= a[sup]) {
            temp[j++] = a[inf++];
        } else {
            temp[j++] = a[sup++];
        }
    }

    /* Copy any remaining elements from the first portion. */
    while (inf <= m) {
        temp[j++] = a[inf++];
    }

    /* Copy any remaining elements from the second portion. */
    while (sup <= f) {
        temp[j++] = a[sup++];
    }

    /* Copy the merged elements back into the original array. */
    for (int k = 0; k < j; k++) {
        a[i + k] = temp[k];
    }
}

/*
 * Merge Sort:
 * recursively divides the array into smaller portions,
 * sorts them, and then merges the sorted portions.
 */
void mergeSort(int *a, int i, int f) {
    if (i >= f) {
        return;
    }

    int m = (i + f) / 2;

    mergeSort(a, i, m);
    mergeSort(a, m + 1, f);
    merge(a, i, m, f);
}

int main(void) {
    int a[] = {7, 2, 4, 5, 3, 1};

    printf("Selection Sort:\n");
    printArray(a, 6);
    selectionSort(a, 6);

    int b[] = {7, 2, 4, 5, 3, 1};

    printf("\nInsertion Sort:\n");
    printArray(b, 6);
    insertionSort(b, 6);

    int c[] = {7, 2, 4, 5, 3, 1};

    printf("\nBubble Sort:\n");
    printArray(c, 6);
    insertionSort(c, 6);

    int d[] = {7, 2, 4, 5, 3, 1};

    printf("\nMerge Sort:\n");
    printArray(d, 6);
    mergeSort(d, 0, 6);
    printArray(d, 6);

    return 0;
}
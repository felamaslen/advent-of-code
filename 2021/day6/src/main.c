#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NUM_DAYS 80
#define INPUT_FILE "./input.txt"
#define MAX_INITIAL_FISH 1000
#define BUFFER_SIZE 1000

int readInitialFish(int** fish) {
  FILE* file = fopen(INPUT_FILE, "r");
  if (file == NULL) {
    printf("Error opening input file\n");
    exit(1);
  }

  char firstLine [MAX_INITIAL_FISH];
  fgets(firstLine, MAX_INITIAL_FISH, file);

  char delim[] = ",";
  char* split = strtok(firstLine, delim);
  int f = 0;
  while (split != NULL) {
    int nextFish = atoi(split);
    (*fish)[f++] = nextFish;
    split = strtok(NULL, delim);
  }

  fclose(file);

  int numInitialFish = f;
  return numInitialFish;
}

void growFish(int** fish, int* numFish, int* bufferSize) {
  int f, numAdditionalFish = 0;
  for (f = 0; f < *numFish; f++) {
    if ((*fish)[f] == 0) {
      if (*numFish + numAdditionalFish >= *bufferSize - 1) {
        *bufferSize += BUFFER_SIZE;
        *fish = realloc(*fish, *bufferSize * sizeof (int));
      }
      (*fish)[f] = 6;
      (*fish)[*numFish + numAdditionalFish] = 8;
      numAdditionalFish++;
    } else {
      (*fish)[f]--;
    }
  }

  *numFish += numAdditionalFish;
}

int growFishForNDays(int* fish, int numFish, int bufferSize, int numDays) {
  int d, f;
  for (d = 0; d < numDays; d++) {
    growFish(&fish, &numFish, &bufferSize);
  }

  return numFish;
}

void task1() {
  int* fish = malloc(BUFFER_SIZE * sizeof (int));
  int numInitialFish = readInitialFish(&fish);
  int initialBufferSize = BUFFER_SIZE;

  int numFishAfterGrowth = growFishForNDays(fish, numInitialFish, initialBufferSize, NUM_DAYS);
  printf("Task 1: numFishAfterGrowth=%d\n", numFishAfterGrowth);
}

int main() {
  task1();
}

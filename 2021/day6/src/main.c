#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INPUT_FILE "./input.txt"
#define MAX_INITIAL_FISH 1000
#define BUFFER_SIZE 1000
#define MAX_N 8

long long* readInitialFish() {
  long long* numOfEachN = malloc((MAX_N + 1) * sizeof (long long));
  for (int n = 0; n <= MAX_N; n++) {
    numOfEachN[n] = 0;
  }

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
    short nextFish = atoi(split);
    numOfEachN[nextFish]++;
    split = strtok(NULL, delim);
  }

  fclose(file);

  return numOfEachN;
}

void growFish(long long** numOfEachN) {
  long long newNumOfEachN[MAX_N + 1];

  newNumOfEachN[0] = (*numOfEachN)[1];
  newNumOfEachN[1] = (*numOfEachN)[2];
  newNumOfEachN[2] = (*numOfEachN)[3];
  newNumOfEachN[3] = (*numOfEachN)[4];
  newNumOfEachN[4] = (*numOfEachN)[5];
  newNumOfEachN[5] = (*numOfEachN)[6];
  newNumOfEachN[6] = (*numOfEachN)[7] + (*numOfEachN)[0];
  newNumOfEachN[7] = (*numOfEachN)[8];
  newNumOfEachN[8] = (*numOfEachN)[0];

  for (int n = 0; n <= MAX_N; n++) {
    (*numOfEachN)[n] = newNumOfEachN[n];
  }
}

long long growFishForNDays(int numDays) {
  long long* numOfEachN = readInitialFish();

  for (int day = 0; day < numDays; day++) {
    growFish(&numOfEachN);
  }

  long long finalNumFish = 0;
  for (int n = 0; n <= MAX_N; n++) {
    finalNumFish += numOfEachN[n];
  }

  free(numOfEachN);
  return finalNumFish;
}

void task1() {
  long long numFishAfterGrowth = growFishForNDays(80);
  printf("Task 1: numFishAfterGrowth=%lld\n", numFishAfterGrowth);
}

void task2() {
  long long numFishAfterGrowth = growFishForNDays(256);
  printf("Task 1: numFishAfterGrowth=%lld\n", numFishAfterGrowth);
}

int main() {
  task1();
  task2();
}

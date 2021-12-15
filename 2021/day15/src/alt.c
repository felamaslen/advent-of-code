#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INPUT "./input_small.txt"

typedef struct {
  int** cells;
  int rows;
  int cols;
} Board_t;

typedef struct {
  int x;
  int y;
} Point_t;

typedef struct {
  Point_t* points;
  int numPoints;
} Path_t;

void freeBoard(Board_t* board) {
  for (int y = 0; y < board->rows; y++) {
    free(board->cells[y]);
  }
  free(board->cells);
  free(board);
}

Board_t* readInput(char* inputFile) {
  FILE* fp = fopen(inputFile, "r");
  if (fp == NULL) {
    fprintf(stderr, "Error opening file\n");
    exit(1);
  }

  ssize_t read;
  char* line = NULL;
  size_t len = 0;

  int** cells = malloc(sizeof(int*));

  int x, y = 0;
  int cols = 0;

  while ((read = getline(&line, &len, fp)) != -1) {
    if (cols > 0) {
      if (cols != read - 1) {
        fprintf(stderr, "Inconsistent row width\n");
        exit(1);
      }
    } else {
      cols = read - 1;
    }

    cells = realloc(cells, (y + 1) * sizeof(int*));
    int* row = malloc(cols * sizeof(int));
    cells[y] = row;

    for (x = 0; x < cols; x++) {
      char cell[1];
      memcpy(cell, &line[x], 1);
      cells[y][x] = atoi(cell);
    }

    y++;
  }

  Board_t* board = malloc(sizeof(Board_t));
  board->cells = cells;
  board->rows = y;
  board->cols = cols;

  return board;
}

void clearBoard(Board_t* board) {
  printf("\33[2K\r");
  for (int y = 0; y < board->rows; y++) {
    printf("\033[A");
  }
}

bool isPointEqual(Point_t* a, Point_t* b) {
  return a->x == b->x && a->y == b->y;
}

bool pointsContainPoint(Point_t* points, int numPoints, Point_t* point) {
  for (int i = 0; i < numPoints; i++) {
    if (isPointEqual(&points[i], point)) {
      return true;
    }
  }
  return false;
}

bool isPointInPath(Path_t* path, int x, int y) {
  if (path == NULL) {
    return false;
  }
  Point_t myPoint = {.x = x, .y = y};
  return pointsContainPoint(path->points, path->numPoints, &myPoint);
}

void printBoard(Board_t* board, Path_t* path) {
  int x, y;
  for (y = 0; y < board->rows; y++) {
    for (x = 0; x < board->cols; x++) {
      if (isPointInPath(path, x, y)) {
        printf("\x1B[33m\033[1m%d\033[22m\x1B[0m", board->cells[y][x]);
      } else {
        printf("%d", board->cells[y][x]);
      }
    }
    printf("\n");
  }
}

int heuristic(Board_t* board, Point_t* point) {
  return board->rows - 1 - point->y + board->cols - 1 - point->x;
}

int** mapBoardToInts(Board_t* board) {
  int** values = malloc(board->rows * sizeof(int*));
  int x;
  for (int y = 0; y < board->rows; y++) {
    values[y] = malloc(board->cols * sizeof(int));
    for (x = 0; x < board->cols; x++) {
      values[y][x] = -1;
    }
  }
  return values;
}

Point_t** mapBoardToPoints(Board_t* board) {
  Point_t** values = malloc(board->rows * sizeof(Point_t*));
  for (int y = 0; y < board->rows; y++) {
    values[y] = malloc(board->cols * sizeof(Point_t));
  }
  return values;
}

void removePoint(Point_t* openPoints, int numPoints, int index) {
  for (int i = index; i < numPoints - 1; i++) {
    openPoints[i] = openPoints[i + 1];
  }
}

int getLowestFPoint(Point_t* openPoints, int numPoints, int** fValues) {
  int lowestFValue = -1, index, i;
  for (i = 0; i < numPoints; i++) {
    if (lowestFValue == -1 || lowestFValue > fValues[openPoints[i].y][openPoints[i].x]) {
      lowestFValue = fValues[openPoints[i].y][openPoints[i].x];
      index = i;
    }
  }
  return index;
}

int getNeighbours(Point_t* neighbours, Board_t* board, Point_t* point) {
  bool isLeft = point->x == 0;
  bool isTop = point->y == 0;
  bool isRight = point->x == board->cols - 1;
  bool isBottom = point->y == board->rows - 1;

  if (isLeft && isTop) {
    neighbours[0].x = point->x + 1;
    neighbours[0].y = point->y;
    neighbours[1].x = point->x;
    neighbours[1].y = point->y + 1;
    return 2;
  }
  if (isTop && isRight) {
    neighbours[0].x = point->x - 1;
    neighbours[0].y = point->y;
    neighbours[1].x = point->x;
    neighbours[1].y = point->y + 1;
    return 2;
  }
  if (isRight && isBottom) {
    neighbours[0].x = point->x - 1;
    neighbours[0].y = point->y;
    neighbours[1].x = point->x;
    neighbours[1].y = point->y - 1;
    return 2;
  }
  if (isBottom && isLeft) {
    neighbours[0].x = point->x + 1;
    neighbours[0].y = point->y;
    neighbours[1].x = point->x;
    neighbours[1].y = point->y - 1;
    return 2;
  }
  if (isLeft) {
    neighbours[0].x = point->x;
    neighbours[0].y = point->y - 1;
    neighbours[1].x = point->x + 1;
    neighbours[1].y = point->y;
    neighbours[2].x = point->x;
    neighbours[2].y = point->y + 1;
    return 3;
  }
  if (isTop) {
    neighbours[0].x = point->x - 1;
    neighbours[0].y = point->y;
    neighbours[1].x = point->x + 1;
    neighbours[1].y = point->y;
    neighbours[2].x = point->x;
    neighbours[2].y = point->y + 1;
    return 3;
  }
  if (isRight) {
    neighbours[0].x = point->x;
    neighbours[0].y = point->y - 1;
    neighbours[1].x = point->x - 1;
    neighbours[1].y = point->y;
    neighbours[2].x = point->x;
    neighbours[2].y = point->y + 1;
    return 3;
  }
  if (isBottom) {
    neighbours[0].x = point->x - 1;
    neighbours[0].y = point->y;
    neighbours[1].x = point->x + 1;
    neighbours[1].y = point->y;
    neighbours[2].x = point->x;
    neighbours[2].y = point->y - 1;
    return 3;
  }
  neighbours[0].x = point->x - 1;
  neighbours[0].y = point->y;
  neighbours[1].x = point->x;
  neighbours[1].y = point->y - 1;
  neighbours[2].x = point->x + 1;
  neighbours[2].y = point->y;
  neighbours[3].x = point->x;
  neighbours[3].y = point->y + 1;
  return 4;
}

bool isOrigin(Point_t* point) {
  return point->x == 0 && point->y == 0;
}

void generatePath(Path_t* path, Point_t* current, Point_t** cameFrom) {
  path->numPoints = 0;
  int capacity = 1;

  Point_t* point = current;
  bool hasOrigin = isOrigin(point);

  while (point != NULL) {
    if (isOrigin(point)) {
      if (hasOrigin) {
        break;
      } else {
        hasOrigin = true;
      }
    }

    path->numPoints++;
    if (path->numPoints > capacity) {
      capacity += 50;
      path->points = realloc(path->points, capacity * sizeof(Point_t));
    }

    path->points[path->numPoints - 1] = *point;
    point = &cameFrom[point->y][point->x];
  }
}

int findOptimalPath(Board_t* board, short verbosity) {
  Point_t* openPoints = malloc(board->rows * board->cols * sizeof(Point_t));
  openPoints[0].x = 0;
  openPoints[0].y = 0;
  int numOpenPoints = 1;

  Point_t** cameFrom = mapBoardToPoints(board);

  int** fValues = mapBoardToInts(board);
  int** gValues = mapBoardToInts(board);

  fValues[0][0] = heuristic(board, &openPoints[0]);
  gValues[0][0] = 0;

  Point_t finalPoint;
  bool reachedFinalPoint = false;
  Point_t current;
  int lowestPointIndex;

  Point_t* neighbours = malloc(4 * sizeof(Point_t));
  int numNeighbours;
  int i, gValueTentative;

  Path_t currentPath;
  currentPath.points = malloc(1 * sizeof(Point_t));

  if (verbosity >= 2) {
    printBoard(board, NULL);
  }

  while (numOpenPoints > 0) {
    lowestPointIndex = getLowestFPoint(openPoints, numOpenPoints, fValues);
    current = openPoints[lowestPointIndex];

    if (verbosity >= 2) {
      generatePath(&currentPath, &current, cameFrom);
      clearBoard(board);
      printBoard(board, &currentPath);
    }

    if (current.x == board->cols - 1 && current.y == board->rows - 1) {
      finalPoint = current;
      reachedFinalPoint = true;

      if (verbosity < 2) {
        generatePath(&currentPath, &current, cameFrom);
      }

      break;
    }

    removePoint(openPoints, numOpenPoints, lowestPointIndex);
    numOpenPoints--;
    numNeighbours = getNeighbours(neighbours, board, &current);

    for (i = 0; i < numNeighbours; i++) {
      gValueTentative = gValues[current.y][current.x] + board->cells[neighbours[i].y][neighbours[i].x];

      if (!isPointEqual(&neighbours[i], &cameFrom[current.y][current.x]) &&
          (gValues[neighbours[i].y][neighbours[i].x] == -1 ||
          gValueTentative < gValues[neighbours[i].y][neighbours[i].x]
      )) {
        cameFrom[neighbours[i].y][neighbours[i].x] = current;

        gValues[neighbours[i].y][neighbours[i].x] = gValueTentative;
        fValues[neighbours[i].y][neighbours[i].x] = gValueTentative + heuristic(board, &neighbours[i]);

        if (!pointsContainPoint(openPoints, numOpenPoints, &neighbours[i])) {
          openPoints[numOpenPoints] = neighbours[i];
          numOpenPoints++;
        }
      }
    }
  }

  free(currentPath.points);
  free(neighbours);
  free(openPoints);
  free(gValues);
  free(fValues);
  free(cameFrom);

  if (!reachedFinalPoint) {
    fprintf(stderr, "Goal not reached\n");
    exit(1);
  }

  if (verbosity >= 1) {
    printBoard(board, &currentPath);
  }

  int finalScore = fValues[finalPoint.y][finalPoint.x];
  return finalScore;
}

void extendBoard(Board_t* topLeft, Board_t* result) {
  int tilesH = 5;
  int tilesV = 5;
  int** cells = malloc(tilesV * topLeft->rows * sizeof(int*));
  int i, j, x, y;

  for (i = 0; i < tilesV; i++) {
    for (y = 0; y < topLeft->rows; y++) {
      cells[i*topLeft->rows + y] = malloc(tilesH * topLeft->cols * sizeof(int));

      for (j = 0; j < tilesH; j++) {
        for (x = 0; x < topLeft->cols; x++) {
          cells[i*topLeft->rows + y][j * topLeft->cols + x] =
            1 + ((topLeft->cells[y][x] - 1 + i + j) % 9);
        }
      }
    }
  }

  result->cells = cells;
  result->rows = tilesV * topLeft->rows;
  result->cols = tilesH * topLeft->cols;
}

void task1(Board_t* board, short verbosity) {
  int finalScore = findOptimalPath(board, verbosity);
  printf("Task 1: final score=%d\n", finalScore);
}

void task2(Board_t* board, short verbosity) {
  Board_t* combinedBoard = malloc(sizeof(Board_t));
  extendBoard(board, combinedBoard);

  int finalScore = findOptimalPath(combinedBoard, verbosity);
  printf("Task 2: final score=%d\n", finalScore);

  freeBoard(combinedBoard);
}

int main(int argc, char** argv) {
  char* input = INPUT;
  short verbosity = 0;
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-v") == 0) {
      if (verbosity < 1) {
        verbosity = 1;
      }
    } else if (strcmp(argv[i], "-vv") == 0) {
      if (verbosity < 2) {
        verbosity = 2;
      }
    } else {
      input = argv[i];
    }
  }

  Board_t* board = readInput(input);

  task1(board, verbosity);
  task2(board, verbosity);

  freeBoard(board);

  return 0;
}

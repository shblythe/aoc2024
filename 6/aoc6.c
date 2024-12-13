#include <stdio.h>
#include <string.h>

#define MAX_MAP 150

typedef struct {
    int x;
    int y;
} Coord;

typedef enum {
    UP,
    RIGHT,
    DOWN,
    LEFT,
    ROUND_AGAIN
} Dir;

typedef struct {
    Coord pos;
    Dir dir;
} Guard;

typedef struct {
    char map[MAX_MAP][MAX_MAP];
    int rows;
    int cols;
} Map;

void print_map(const Map *map) {
    for (int r=0; r<map->rows; r++) {
        for (int c=0; c<map->cols; c++) {
            printf("%c", map->map[r][c]);
        }
        printf("\n");
    }
}

void print_guard(const Guard *guard) {
    char dir='X';
    switch (guard->dir) {
        case UP: dir='^'; break;
        case DOWN: dir='v'; break;
        case LEFT: dir='<'; break;
        case RIGHT: dir='>'; break;
    };
    printf("(%d, %d) %c\n", guard->pos.x, guard->pos.y, dir);
}

void turn(Dir *dir) {
    (*dir)++;
    if (*dir == ROUND_AGAIN)
        *dir = UP;
}

char map_char(Map map, Coord coord) {
    return map.map[coord.y][coord.x];
}

// Returns 1 if we're still in the map, 0 otherwise
int step(Map map, Guard *guard) {
    // print_guard(guard);
    Coord new_pos = guard->pos;
    switch (guard->dir) {
        case UP: new_pos.y--; break;
        case DOWN: new_pos.y++; break;
        case LEFT: new_pos.x--; break;
        case RIGHT: new_pos.x++; break;
    }
    if (new_pos.y<0 || new_pos.y>=map.rows || new_pos.x<0 || new_pos.x>=map.cols)
        return 0;
    if (map_char(map, new_pos) == '#')
    {
        turn(&guard->dir);
    }
    else
        guard->pos = new_pos;
    return 1;
}

void mark_map(Map *map, Coord pos, char mark) {
    map->map[pos.y][pos.x] = mark;
}

int count_map_marks(Map map) {
    int sum=0;
    for (int x=0; x<map.cols; x++) {
        for (int y=0; y<map.rows; y++) {
            sum+=map.map[y][x];
        }
    }
    return sum;
}

int clear_map_marks(Map *map) {
    for (int x=0; x<map->cols; x++) {
        for (int y=0; y<map->rows; y++) {
            map->map[y][x]='\0';
        }
    }
}

void walk(Map map, Guard guard) {
    Map route = map;
    clear_map_marks(&route);
    do {
        mark_map(&route, guard.pos, 1);
    } while (step(map, &guard));
    printf("Part 1: %d\n", count_map_marks(route));
}

// Returns 1 if we're stuck in an infinite loop
// ** No!  Just checking against the start position doesn't work.
// ** We might end up in a different loop!
int walk2(Map map, Guard guard, Coord new_obstacle) {
    Map route = map;
    map.map[new_obstacle.y][new_obstacle.x] = '#';
    printf("%d %d\n", new_obstacle.x, new_obstacle.y);
    // print_map(&map);
    Guard start = guard;
    int on_map;
    int count = 0;
    do {
        count++;
        if (count == 10000)
            start = guard;
        on_map = step(map, &guard);
    } while (
        on_map &&
        (
            guard.pos.x != start.pos.x ||
            guard.pos.y != start.pos.y ||
            guard.dir != start.dir
        )
    );
    // if (on_map==1)
    //     printf("Looped! %d %d\n", new_obstacle.x, new_obstacle.y);
    return on_map;
}

int main(int argc, char **argv) {
    FILE* f = fopen(argv[1], "r");
    int row;
    Guard guard;
    Map map;
    for (row = 0; !feof(f); row++) {
        fgets(map.map[row], MAX_MAP, f);
        if (map.map[row][strlen(map.map[row])-1] == '\n')
            map.map[row][strlen(map.map[row])-1] = '\0';
        char* gpos = strchr(map.map[row], '^');
        if (gpos > 0) {
            guard.pos.y = row;
            guard.pos.x = gpos - map.map[row];
        }
    }
    fclose(f);
    guard.dir = UP;
    map.rows=row;
    map.cols=strlen(map.map[0]);
    //print_map(&map);
    walk(map, guard);

    int sum = 0;
    for (int x=0; x<map.cols; x++) {
        for (int y=0; y<map.rows; y++) {
            Coord new_obstacle = { .x = x, .y = y };
            // printf("%d %d\n", x, y);
            sum += walk2(map, guard, new_obstacle);
        }
    }
    printf("Part2: %d\n", sum);

    return(0);
}
import random
import numpy as np
import collections
from enum import Enum


class Square(Enum):
    NOTHING = 0
    CAT = 1
    FOOD = 2
    DOG = 3
    WALL = 4


class Direction(Enum):
    UP = 0
    DOWN = 1
    LEFT = 2
    RIGHT = 3


class City:
    """
    Observation: 3 squares front, one square left, one square right 
    Actions: nothing, turn left, turn right
    At each time step the cat moves 1 square forward, and scores 1 point per move, but losses 1 point of vitality.
    It also gains 10 points per food and -5 per wall.
    If the cat meets a dog, game is over and it loses 10 points.
    If the cat finds a food spot, it gains +10 of vitality
    """
    def __init__(self, width: int = 30, height: int = 20, vitality: int = 30):
        self.width: int = width + 2
        self.height: int = height + 2
        self.object_capacity: int = (self.width + self.height) // 2
        self.city: np.ndarray = np.empty((self.height, self.width), dtype=object)
        self.city.fill(Square.NOTHING)
        self.foods: collections.deque = collections.deque(maxlen=self.object_capacity)
        self.dogs: collections.deque = collections.deque(maxlen=self.object_capacity)
        self.done: bool = False
        self.score: int = 0
        self.cat_x: int = -1
        self.cat_y: int = -1
        self.direction: Direction = Direction.UP
        self.starting_vitality: int = vitality
        self.vitality: int = vitality
        self.move: int = -1
        self.actions: np.list = ['no', 'left', 'right']
        self.observations: np.list = self.reset()

    def reset(self):
        self.score = 0
        self.direction: Direction = Direction.UP
        self.done = False
        self.vitality = self.starting_vitality
        self.city.fill(Square.NOTHING)
        self.add_buildings()
        self.cat_x = self.width // 2 + random.randint(-2, 2)
        self.cat_y = self.height // 2 + random.randint(-2, 2)
        self.city[self.cat_y, self.cat_x] = Square.CAT
        self.foods.clear()
        self.dogs.clear()
        for _ in range(self.object_capacity // 2):
            self.add_food()
        for _ in range(self.object_capacity // 2):
            self.add_dog()
        return self.generate_observations()

    def add_buildings(self):
        for i in range(self.width):
            self.city[0, i] = Square.WALL
            self.city[self.height - 1, i] = Square.WALL
        for i in range(self.height):
            self.city[i, 0] = Square.WALL
            self.city[i, self.width - 1] = Square.WALL

    def add_food(self):
        added: bool = False
        if len(self.foods) == self.object_capacity:
            (tmp_y, tmp_x) = self.foods.popleft()
            assert self.city[tmp_y, tmp_x] == Square.FOOD
            self.city[tmp_y, tmp_x] = Square.NOTHING
        while not added:
            x = random.randint(1, self.width - 2)
            y = random.randint(1, self.height - 2)
            if self.city[y, x] == Square.NOTHING:
                self.city[y, x] = Square.FOOD
                self.foods.append((y, x))
                added = True

    def add_dog(self):
        added: bool = False
        if len(self.dogs) == self.object_capacity:
            (tmp_y, tmp_x) = self.dogs.popleft()
            assert self.city[tmp_y, tmp_x] == Square.DOG
            self.city[tmp_y, tmp_x] = Square.NOTHING
        while not added:
            x = random.randint(1, self.width - 2)
            y = random.randint(1, self.height - 2)
            if self.city[y, x] == Square.NOTHING:
                self.city[y, x] = Square.DOG
                self.dogs.append((y, x))
                added = True

    def cat_move(self):
        self.vitality -= 1
        self.score += 1
        if self.vitality == 0:
            self.done = True
        if self.direction == Direction.UP:
            if self.cat_y > 0:
                return self.cat_x, self.cat_y - 1
        if self.direction == Direction.DOWN:
            if self.cat_y < self.height - 1:
                return self.cat_x, self.cat_y + 1
        if self.direction == Direction.LEFT:
            if self.cat_x > 0:
                return self.cat_x - 1, self.cat_y
        if self.direction == Direction.RIGHT:
            if self.cat_x < self.width - 1:
                return self.cat_x + 1, self.cat_y
        return self.cat_x, self.cat_y

    def get_front_info(self):
        # 3 cases front
        front: np.list = []
        i = self.cat_x
        j = self.cat_y
        for _ in range(0, 3):
            if self.direction == Direction.UP:
                j -= 1
            if self.direction == Direction.DOWN:
                j += 1
            if self.direction == Direction.LEFT:
                i -= 1
            if self.direction == Direction.RIGHT:
                i += 1
            if self.coordinate_in_city(i, j):
                front.append(self.city[j, i])
            else:
                front.append(Square.WALL)
        return front

    def get_left_info(self):
        # 1 case left
        i = self.cat_x
        j = self.cat_y
        if self.direction == Direction.UP:
            i -= 1
        if self.direction == Direction.DOWN:
            i += 1
        if self.direction == Direction.LEFT:
            j += 1
        if self.direction == Direction.RIGHT:
            j -= 1
        if self.coordinate_in_city(i, j):
            return self.city[j, i]
        else:
            return Square.WALL

    def get_right_info(self):
        # 1 case right
        i = self.cat_x
        j = self.cat_y
        if self.direction == Direction.UP:
            i += 1
        if self.direction == Direction.DOWN:
            i -= 1
        if self.direction == Direction.LEFT:
            j -= 1
        if self.direction == Direction.RIGHT:
            j += 1
        if self.coordinate_in_city(i, j):
            return self.city[j, i]
        else:
            return Square.WALL

    def update_direction(self, move: str):
        assert move in self.actions
        if move == 'no':
            return
        if move == 'right':
            if self.direction == Direction.UP:
                self.direction = Direction.RIGHT
            elif self.direction == Direction.RIGHT:
                self.direction = Direction.DOWN
            elif self.direction == Direction.DOWN:
                self.direction = Direction.LEFT
            else:
                self.direction = Direction.UP
            return
        if move == 'left':
            if self.direction == Direction.UP:
                self.direction = Direction.LEFT
            elif self.direction == Direction.LEFT:
                self.direction = Direction.DOWN
            elif self.direction == Direction.DOWN:
                self.direction = Direction.RIGHT
            else:
                self.direction = Direction.UP

    def step(self, move: str):
        old_score = self.score
        self.update_direction(move)
        new_x, new_y = self.cat_move()
        if not self.done:
            self.check_collisions(new_x, new_y)
        if not self.done:
            self.add_food()
            self.add_dog()
        if not self.done:
            if self.score > 100:
                self.done = True
        self.observations = self.generate_observations()
        return self.observations, self.score - old_score, self.done

    def coordinate_in_city(self, x: int, y: int):
        return 0 <= x < self.width and 0 <= y < self.height

    def check_collisions(self, x, y):
        if self.city[y, x] == Square.DOG:
            self.score -= 5
            self.done = True
            return
        if self.city[y, x] == Square.WALL:
            self.score -= 2
            return
        if self.city[y, x] == Square.FOOD:
            self.foods.remove((y, x))
            self.score += 10
            self.vitality += 5
        self.city[self.cat_y, self.cat_x] = Square.NOTHING
        self.cat_x = x
        self.cat_y = y
        self.city[self.cat_y, self.cat_x] = Square.CAT

    def get_observations(self):
        # 3 cases front
        obs: np.list = self.get_front_info()
        # 1 case left
        obs.append(self.get_left_info())
        obs.append(self.get_right_info())
        obs.append(self.vitality)
        assert len(obs) == 6
        return obs

    def generate_observations(self):
        obs = self.get_observations()
        first_next_front_is_food: int = obs[0] == Square.FOOD
        first_next_front_is_dog: int = obs[0] == Square.DOG
        first_next_front_is_wall: int = obs[0] == Square.WALL
        second_next_front_is_food: int = obs[1] == Square.FOOD
        second_next_front_is_dog: int = obs[1] == Square.DOG
        second_next_front_is_wall: int = obs[1] == Square.WALL
        third_next_front_is_food: int = obs[2] == Square.FOOD
        third_next_front_is_dog: int = obs[2] == Square.DOG
        third_next_front_is_wall: int = obs[2] == Square.WALL
        next_left_is_food: int = obs[3] == Square.FOOD
        next_left_is_dog: int = obs[3] == Square.DOG
        next_left_is_wall: int = obs[3] == Square.WALL
        next_right_is_food: int = obs[4] == Square.FOOD
        next_right_is_dog: int = obs[4] == Square.DOG
        next_right_is_wall: int = obs[4] == Square.WALL
        return [
            first_next_front_is_food, first_next_front_is_dog, first_next_front_is_wall,
            second_next_front_is_food, second_next_front_is_dog, second_next_front_is_wall,
            third_next_front_is_food, third_next_front_is_dog, third_next_front_is_wall,
            next_left_is_food, next_left_is_dog, next_left_is_wall,
            next_right_is_food, next_right_is_dog, next_right_is_wall]

    def direction_to_str(self):
        return self.direction.name

    def __str__(self):
        city_str: str = ''
        for j in range(self.height):
            for i in range(self.width):
                if self.city[j, i] == Square.CAT:
                    city_str += 'C'
                elif self.city[j, i] == Square.FOOD:
                    city_str += 'F'
                elif self.city[j, i] == Square.DOG:
                    city_str += 'D'
                elif self.city[j, i] == Square.WALL:
                    city_str += '#'
                else:
                    city_str += ' '
            city_str += '\n'
        print("Cat in : (", self.cat_x, ", ", self.cat_y, ")")
        print("Direction: ", self.direction_to_str(), ", Score: ", self.score, ", Vitality: ", self.vitality)
        return city_str


if __name__ == '__main__':
    city = City()
    print(city)

import numpy as np
import sys

def find_bingos(draws, boards):
    for draw in draws:
        boards = np.ma.masked_equal(boards, value=draw)
        board_masks = np.ma.getmask(boards)
        bingos = np.any(np.logical_or(np.all(board_masks, axis=1), np.all(board_masks, axis=2)), axis=1)
        for board in boards[bingos]:
            yield draw, board
        boards = boards[np.logical_not(bingos)]

def bingo_score(bingo):
    draw, board = bingo
    score = draw * np.sum(board)
    return score

if __name__ == '__main__':
    input_s = sys.stdin.read()
    draws_s, *boards_s = input_s.split('\n\n')
    draws = np.array([int(draw_s) for draw_s in draws_s.split(',')])
    boards = np.array([[[int(cell_s) for cell_s in row_s.split()] for row_s in board_s.splitlines()] for board_s in boards_s])

    best_bingo, *_, worst_bingo = find_bingos(draws, boards)

    print(bingo_score(best_bingo))
    print(bingo_score(worst_bingo))

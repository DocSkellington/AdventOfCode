def get_win(move: str) -> str:
    match move:
        case "A":
            return "Y"
        case "B":
            return "Z"
        case "C":
            return "X"
        case "X":
            return "C"
        case "Y":
            return "A"
        case "Z":
            return "B"


def get_draw(move: str) -> str:
    match move:
        case "A":
            return "X"
        case "B":
            return "Y"
        case "C":
            return "Z"
        case "X":
            return "A"
        case "Y":
            return "B"
        case "Z":
            return "C"


def get_loss(move: str) -> str:
    match move:
        case "A":
            return "Z"
        case "B":
            return "X"
        case "C":
            return "Y"
        case "Z":
            return "A"
        case "X":
            return "B"
        case "Y":
            return "C"

def score_moves(opponent: str, player: str) -> str:
    score = 0
    if player == "X":
        score += 1
        if opponent == "A":
            score += 3
        elif opponent == "C":
            score += 6
    elif player == "Y":
        score += 2
        if opponent == "B":
            score += 3
        elif opponent == "A":
            score += 6
    elif player == "Z":
        score += 3
        if opponent == "C":
            score += 3
        elif opponent == "B":
            score += 6
    return score


def score_end(opponent, end):
    if end == "Y":
        return score_moves(opponent, get_draw(opponent))
    elif end == "X":
        return score_moves(opponent, get_loss(opponent))
    elif end == "Z":
        return score_moves(opponent, get_win(opponent))


with open("input", "r") as f:
    total_1 = total_2 = 0
    for line in f.readlines():
        (opponent, player) = line.strip().split(" ")
        total_1 += score_moves(opponent, player)
        total_2 += score_end(opponent, player)
    print(total_1, total_2)
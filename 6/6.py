def task(line: str, number_different: int) -> None:
    for i in range(len(line)):
        last = ""
        for j in range(i, len(line)):
            if line[j] in last:
                break
            elif len(last) == number_different - 1:
                print(i + number_different)
                return
            else:
                last += line[j]

with open("input", "r") as f:
    line = f.readlines()[0].strip()

    # Task 1
    task(line, 4)

    # Task 2
    task(line, 14)
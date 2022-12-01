with open("input", "r") as inF:
    with open("input.scm", "w") as outF:
        outF.write("(define data '(")
        elf = []
        for line in inF.readlines():
            if line == "\n":
                outF.write("(" + " ".join(elf) + ") ")
                elf = []
            else:
                elf.append(line.strip())
        outF.write("))")
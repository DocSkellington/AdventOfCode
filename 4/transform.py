with open("input", "r") as inF:
    with open("input.scm", "w") as outF:
        outF.write("(define data (list ")
        for line in inF.readlines():
            elf1, elf2 = line.strip().split(",")
            elf1Start, elf1Stop = elf1.strip().split("-")
            elf2Start, elf2Stop = elf2.strip().split("-")
            outF.write("(cons (cons {} {}) (cons {} {})) ".format(elf1Start, elf1Stop, elf2Start, elf2Stop))
        outF.write("))")
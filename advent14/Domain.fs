module Domain
type Op = Hi | Low | Unchanged
type Bitmask = Op array 
type LoadToMem = int * int
type Instruction = Mask of Bitmask | MemInstr of LoadToMem 
type Program = Instruction list

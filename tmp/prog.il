Compiled Ruleset @602 
Compiled Rule 
	--atommatch:
	--memmatch:
		spec           [1, 1]
		jump           [L131, [0], [], []]
	--guard:L131:
		spec           [1, 1]
		jump           [L132, [0], [], []]
	--body:L132:
		spec           [1, 4]
		commit         ["_init", 0]
		loadruleset    [0, @601]
		newatom      [1, 0, 't5'_0]
		newatom      [2, 0, 't5'_0]
		newatom      [3, 0, 't5'_0]
		proceed        []


Compiled Ruleset @601      
Compiled Rule 
	--atommatch:
	--memmatch:
		spec           [1, 11]
		findatom     [1, 0, 't5'_0]
		commit         ["t5", 0]
		removeatom     [1, 0]
		newatom      [2, 0, 't4'_0]
		newatom      [3, 0, 't4'_0]
		newatom      [4, 0, 't4'_0]
		newatom      [5, 0, 't4'_0]
		newatom      [6, 0, 't4'_0]
		newatom      [7, 0, 't4'_0]
		newatom      [8, 0, 't4'_0]
		newatom      [9, 0, 't4'_0]
		newatom      [10, 0, 't4'_0]
		freeatom       [1]
		proceed        []

Compiled Rule 
	--atommatch:
	--memmatch:
		spec           [1, 11]
		findatom     [1, 0, 't4'_0]
		commit         ["t4", 0]
		removeatom     [1, 0]
		newatom      [2, 0, 't3'_0]
		newatom      [3, 0, 't3'_0]
		newatom      [4, 0, 't3'_0]
		newatom      [5, 0, 't3'_0]
		newatom      [6, 0, 't3'_0]
		newatom      [7, 0, 't3'_0]
		newatom      [8, 0, 't3'_0]
		newatom      [9, 0, 't3'_0]
		newatom      [10, 0, 't3'_0]
		freeatom       [1]
		proceed        []

Compiled Rule 
	--atommatch:
	--memmatch:
		spec           [1, 11]
		findatom     [1, 0, 't3'_0]
		commit         ["t3", 0]
		removeatom     [1, 0]
		newatom      [2, 0, 't2'_0]
		newatom      [3, 0, 't2'_0]
		newatom      [4, 0, 't2'_0]
		newatom      [5, 0, 't2'_0]
		newatom      [6, 0, 't2'_0]
		newatom      [7, 0, 't2'_0]
		newatom      [8, 0, 't2'_0]
		newatom      [9, 0, 't2'_0]
		newatom      [10, 0, 't2'_0]
		freeatom       [1]
		proceed        []

Compiled Rule 
	--atommatch:
	--memmatch:
		spec           [1, 11]
		findatom     [1, 0, 't2'_0]
		commit         ["t2", 0]
		removeatom     [1, 0]
		newatom      [2, 0, 't1'_0]
		newatom      [3, 0, 't1'_0]
		newatom      [4, 0, 't1'_0]
		newatom      [5, 0, 't1'_0]
		newatom      [6, 0, 't1'_0]
		newatom      [7, 0, 't1'_0]
		newatom      [8, 0, 't1'_0]
		newatom      [9, 0, 't1'_0]
		newatom      [10, 0, 't1'_0]
		freeatom       [1]
		proceed        []

Compiled Rule 
	--atommatch:
	--memmatch:
		spec           [1, 11]
		findatom     [1, 0, 't1'_0]
		commit         ["t1", 0]
		removeatom     [1, 0]
		newatom      [2, 0, 't0'_0]
		newatom      [3, 0, 't0'_0]
		newatom      [4, 0, 't0'_0]
		newatom      [5, 0, 't0'_0]
		newatom      [6, 0, 't0'_0]
		newatom      [7, 0, 't0'_0]
		newatom      [8, 0, 't0'_0]
		newatom      [9, 0, 't0'_0]
		newatom      [10, 0, 't0'_0]
		freeatom       [1]
		proceed        []

Compiled Rule 
	--atommatch:
	--memmatch:
		spec           [1, 2]
		findatom     [1, 0, 't0'_0]
		commit         ["delete", 0]
		removeatom     [1, 0]
		freeatom       [1]
		proceed        []



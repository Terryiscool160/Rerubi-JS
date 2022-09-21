/*
  Rerubi-JS
  
  Unfinished
  
  A partial construction of Rerumu's Rerubi
  but Javascript
*/

const Opmode = [
    { b: 'OpArgR', c: 'OpArgN' }, { b: 'OpArgK', c: 'OpArgN' }, { b: 'OpArgU', c: 'OpArgU' },
    { b: 'OpArgR', c: 'OpArgN' }, { b: 'OpArgU', c: 'OpArgN' }, { b: 'OpArgK', c: 'OpArgN' },
    { b: 'OpArgR', c: 'OpArgK' }, { b: 'OpArgK', c: 'OpArgN' }, { b: 'OpArgU', c: 'OpArgN' },
    { b: 'OpArgK', c: 'OpArgK' }, { b: 'OpArgU', c: 'OpArgU' }, { b: 'OpArgR', c: 'OpArgK' },
    { b: 'OpArgK', c: 'OpArgK' }, { b: 'OpArgK', c: 'OpArgK' }, { b: 'OpArgK', c: 'OpArgK' },
    { b: 'OpArgK', c: 'OpArgK' }, { b: 'OpArgK', c: 'OpArgK' }, { b: 'OpArgK', c: 'OpArgK' },
    { b: 'OpArgR', c: 'OpArgN' }, { b: 'OpArgR', c: 'OpArgN' }, { b: 'OpArgR', c: 'OpArgN' },
    { b: 'OpArgR', c: 'OpArgR' }, { b: 'OpArgR', c: 'OpArgN' }, { b: 'OpArgK', c: 'OpArgK' },
    { b: 'OpArgK', c: 'OpArgK' }, { b: 'OpArgK', c: 'OpArgK' }, { b: 'OpArgR', c: 'OpArgU' },
    { b: 'OpArgR', c: 'OpArgU' }, { b: 'OpArgU', c: 'OpArgU' }, { b: 'OpArgU', c: 'OpArgU' },
    { b: 'OpArgU', c: 'OpArgN' }, { b: 'OpArgR', c: 'OpArgN' }, { b: 'OpArgR', c: 'OpArgN' },
    { b: 'OpArgN', c: 'OpArgU' }, { b: 'OpArgU', c: 'OpArgU' }, { b: 'OpArgN', c: 'OpArgN' },
    { b: 'OpArgU', c: 'OpArgN' }, { b: 'OpArgU', c: 'OpArgN' }
]

const Opcode = [
    'ABC', 'ABx', 'ABC', 'ABC',
    'ABC', 'ABx', 'ABC', 'ABx',
    'ABC', 'ABC', 'ABC', 'ABC',
    'ABC', 'ABC', 'ABC', 'ABC',
    'ABC', 'ABC', 'ABC', 'ABC',
    'ABC', 'ABC', 'AsBx', 'ABC',
    'ABC', 'ABC', 'ABC', 'ABC',
    'ABC', 'ABC', 'ABC', 'AsBx',
    'AsBx', 'ABC', 'ABC', 'ABC',
    'ABx', 'ABC',
]

//  rlbi author -> Rerumu
//  special thanks;
//  @cntkillme for providing faster bit extraction
//  @Eternal for being #1 bug finder and providing better float decoder
//  @stravant for contributing to the original project this is derived from

/* 
    TODO:
    for some reason my local bytecode isnt being processed properly?
*/

const GBit = function(Bit, Start, End) { // No tail-calls, yay.
    if (End) {
        let Response = (Bit / Math.pow(2, Start - 1)) % Math.pow(2, (End - 1) - (Start - 1) + 1)

        return Response - Response % 1
    } else {
        let Plc = Math.pow(2, Start - 1)

        return (Bit % (Plc + Plc) >= Plc) && 1 || 0
    }
}

const ldexp = function(Mantissa, Exponent) {
    let Steps = Math.min(3, Math.ceil(Math.abs(Exponent) / 1023));
    let Result = Mantissa;

    for (let I = 0; I < Steps; I++) {
        Result *= Math.pow(2, Math.floor((Exponent + I) / Steps));
    }

    return Result;
}

// GetMeaning
const GetMeaning = function(Bytecode) {
    let Pos = 0; // lua
    let GSizeT;
    let GInt;

    const GBits8 = function() {
        let F = Bytecode.charCodeAt(Pos);

        Pos++;

        return F;
    }

    const GBits32 = function() {
        const W = Bytecode.charCodeAt(Pos)
        const X = Bytecode.charCodeAt(Pos + 1)
        const Y = Bytecode.charCodeAt(Pos + 2)
        const Z = Bytecode.charCodeAt(Pos + 3)

        console.log(W, X, Y, Z)

        Pos = Pos + 4

        return (Z * 16777216) + (Y * 65536) + (X * 256) + W;
    }

    const GBits64 = function() {
        return GBits32() * 4294967296 + GBits32()
    }

    const GFloat = function() {
        let Left = GBits32()
        let Right = GBits32()
        let IsNormal = 1
        let Mantissa = (GBit(Right, 1, 20) * Math.pow(2, 32)) + Left

        let Exponent = GBit(Right, 21, 31)
        let Sign = Math.pow(-1, GBit(Right, 32))

        if (Exponent == 0) {
            if (Mantissa == 0) {
                return Sign * 0 // +-0
            } else {
                Exponent = 1
                IsNormal = 0
            }
        } else if (Exponent == 2047) {
            if (Mantissa == 0) {
                return Sign * (1 / 0) // +-Inf
            } else {
                return Sign * (0 / 0) // +-Q/Nan
            }
        }

        // sign * 2**e-1023 * isNormal.mantissa
        return ldexp(Sign, Exponent - 1023) * (IsNormal + (Mantissa / Math.pow(2, 52)))
    }

    const GString = function(Length) {
        let Str

        if (Length) {
            Str = Bytecode.substring(Pos, Pos + Length)

            Pos = Pos + Length
        } else {
            Length = GSizeT()

            if (Length == 0) return

            Str = Bytecode.substring(Pos, Pos + Length)

            Pos = Pos + Length
        }

        return Str;
    };

    const ChunkDecode = function() {
        let Instr = {};
        let Const = {};
        let Proto = {};

        let Chunk = {
			Instr:  Instr, // Instructions
			Const:  Const, // Constants
			Proto:  Proto, // Prototypes
			Line:   {}, // Lines
			Name:   GString(), // Grab name string.
			FirstL:	GInt(), // First line.
			LastL:	GInt(), // Last line.
			Upvals:	GBits8(), // Upvalue count.
			Args:	GBits8(), // Arg count.
			Vargs:	GBits8(), // Vararg type.
			Stack:	GBits8() // Stack.
        };

        let ConstantReferences = {};

        if (Chunk.Name) {
            // Chunk.Name = string.sub(Chunk.Name, 1, -2);
            Chunk.Name = Chunk.Name.substring(0, Chunk.Name.length - 1)
        };

        // we have to store this here because
        // js loops are not lua loops
        // so sad right

        // for Idx = 1, GInt() do 
        let Value = GInt()

        for (let Idx = 1; Idx <= Value; Idx++) { // Loading Instructions to the chunk.
            let Data = GBits32()
            let Opco = GBit(Data, 1 , 6)
            let Type = Opcode[Opco]
            let Mode = Opmode[Opco]

            let Inst = {
                Value: Data,
                Enum: Opco,
                [1]: GBit(Data, 7, 14) 
            }

            if (Type == 'ABC') { // Most common, basic instruction type.
                Inst[2]     = GBit(Data, 24, 32)
                Inst[3]     = GBit(Data, 15, 23)
            } else if (Type == 'ABx') {
                Inst[2]     = GBit(Data, 15, 32)
            } else if (Type == 'AsBx') {
                Inst[2]     = GBit(Data, 15, 32) - 131071
            }

            // Precompute data for some Instructions

            // TEST & TESTSET
            if (Opco == 26 || Opco == 27) {
                Inst[3] = Inst[3] == 0
            }

            // EQ & LT & LE
            if (Opco >= 23 && Opco <= 25) {
                Inst[1] = Inst[1] != 0
            }
            
            // Anything that looks at a constant using B
            if (Mode.b == 'OpArgK') {
                Inst[3] = Inst[3] || false // Simply to guarantee that Inst[4] is inserted in the array part

                if (Inst[2] >= 256) {
                    let Cons = Inst[2] - 256
                    Inst[4] = Cons

                    let ReferenceData = ConstantReferences[Cons];

                    if (!ReferenceData) {
                        ReferenceData = [];
                        ConstantReferences[Cons] = ReferenceData;
                    }

                    ReferenceData[ReferenceData.length + 1] = { Inst: Inst, Register: 4 };
                }
            }

            // Anything that looks at a constant using C
            if (Mode.c == 'OpArgK') {
                Inst[4] = Inst[4] || false // Simply to guarantee that Inst[5] is inserted in the array part

                if (Inst[3] >= 256) {
                    let Cons = Inst[3] - 256
                    Inst[5] = Cons

                    let ReferenceData = ConstantReferences[Cons];

                    if (!ReferenceData) {
                        ReferenceData = [];
                        ConstantReferences[Cons] = ReferenceData;
                    }

                    ReferenceData[ReferenceData.length + 1] = { Inst: Inst, Register: 5 };
                }
            }

            Instr[Idx] = Inst;
        };

        // said why before
        Value = GInt()

        for (let Idx = 1; Idx <= Value; Idx++) { // Load constants.
            let Type = GBits8();
            let Cons;

            if (Type == 1) { // Boolean
                Cons = GBits8() != 0
            } else if (Type == 3) { // Float / Double
                Cons = GFloat()
            } else if (Type == 4) {
                let String = GString()

                // average javascript
                Cons = String.substring(0, String.length - 1)
            }

            // Finish precomputing constants
            let Refs = ConstantReferences[Idx - 1]

            if (Refs) {
                for (let I = 1; I <= Refs.length; I++) {
                    // LUA STARTS ITS TABLES AT 1 DUH
                    Refs[I - 1].Inst[Refs[I - 1].Register] = Cons
                }
            }

            // Write Constant to pool
            Const[Idx] = Cons
        }

        Value = GInt()

        for (let Idx = 1; Idx <= Value; Idx++) { // Nested function Prototypes.
            Proto[Idx] = ChunkDecode()
        }

        console.log(Chunk)

        // Debugging
        let Lines = Chunk.Lines

        Values = GInt()

        for (let Idx = 1; Idx <= Values; Idx++) {
            Lines[Idx] = GBits32()
        }

        Values = GInt()

        for (let Idx = 1; Idx <= Values; Idx++) { // lets in stack.
            console.log(GString()) // Name of lets.
            console.log(GBits32()) // Starting point.
            console.log(GBits32()) // End point.
        }

        Values = GInt()

        for (let Idx = 1; Idx <= Values; Idx++) { // Upvalues
            console.log(GString()) // Name of Upvalue
        }
        // Debugging end

        return Chunk; // Finished chunk
    }

    let Header = GString(4)
    let Version = GBits8()

    console.log('Header: ' + Header)
    console.log('Version: ' + Version)

    GBits8() // Probably Version control.
    GBits8() // Is small endians.

    let IntSize = GBits8() // Int size
    let Sizet = GBits8() // size_t

    console.log('IntSize: ' + IntSize)
    console.log('Sizet: ' + Sizet)

    if (IntSize == 4) {
        GInt = GBits32
    } else if (IntSize == 8) {
        GInt = GBits64
    } else {
        return console.error('Integer size not supported')
    };

    if (Sizet == 4) {
        GSizeT = GBits32
    } else if (Sizet == 8) {
        GSizeT = GBits64
    } else {
        return console.error('Sizet size not supported')
    }

    let Platform = GString(3)

    console.log('Platform: ' + Platform)

    return ChunkDecode()
}

const Wrap = function(Chunk, Env, Upvalues) {
    let Instr = Chunk.Instr;
    let Const = Chunk.Const;
    let Proto = Chunk.Proto;

    const OnError = function(Err, Position) { // Handle your errors in whatever way.
        const Name  = Chunk.Name || 'Code';
        const Line  = Chunk.Lines[Position] || '?';

        console.log(Name + ':' + Line + ': ' + toString(Err))
    };

    function Wrapper() {
        // Get arguments
        let Args = Array.from(arguments);

        // Returned function to run bytecode chunk
        let InstrPoint = 1;
        let Top = -1

        let Varargsz = Args.length;
        let Vararg	= {};

        let GStack  = {};
        let Stack   = GStack;

        const Loop = function() {
            let Inst, Enum;

            while (true) {
				Inst		= Instr[InstrPoint];
				Enum		= Inst.Enum;

				InstrPoint++;
				
				if (Enum == 0) { // MOVE
					Stack[Inst[1]]	= Stack[Inst[2]];
				} else if (Enum == 1) { // LOADK
					Stack[Inst[1]]	= Const[Inst[2]];
				} else if (Enum == 2) { // LOADBOOL
					Stack[Inst[1]]	= (Inst[2] != 0);

                    Inst[3] = Inst[3] != 0 && InstrPoint++
				} else if (Enum == 3) { // LOADNIL
					let Stk	= Stack;

                    /*
                    	for Idx = Inst[1], Inst[2] do
                            Stk[Idx]	= null;
                        end;
                    */

                    for (let Idx = Inst[1]; Inst[1] <= Inst[2]; Idx++) { 
                        Stk[Idx] = null;
                    }
				} else if (Enum == 4) { // GETUPVAL
					Stack[Inst[1]]	= Upvalues[Inst[2]];
				} else if (Enum == 5) { // GETGLOBAL
					Stack[Inst[1]]	= Env[Const[Inst[2]]];
				} else if (Enum == 6) { // GETTABLE
					let Stk	= Stack;

					Stk[Inst[1]] = Stk[Inst[2]][Inst[5] || Stk[Inst[3]]];
				} else if (Enum == 7) { // SETGLOBAL
					Env[Const[Inst[2]]]	= Stack[Inst[1]];
				} else if (Enum == 8) { // SETUPVAL
					Upvalues[Inst[2]]	= Stack[Inst[1]];
				} else if (Enum == 9) { // SETTABLE
					let Stk = Stack

					Stk[Inst[1]][Inst[4] || Stk[Inst[2]]] = Inst[5] || Stk[Inst[3]]
				} else if (Enum == 10) { // NEWTABLE
					Stack[Inst[1]]	= {};
				} else if (Enum == 11) { // SELF
					let Stk	    = Stack;
					let A		= Inst[1];
					let B		= Stk[Inst[2]];
					let C		= Inst[5] || Stk[Inst[3]];

					Stk[A++]	= B;
					Stk[A]		= B[C];
				} else if (Enum == 12) { // ADD
					let Stk = Stack;

					Stk[Inst[1]]	= (Inst[4] || Stk[Inst[2]]) + (Inst[5] || Stk[Inst[3]]);
				} else if (Enum == 13) { // SUB
					let Stk = Stack;

					Stk[Inst[1]]	= (Inst[4] || Stk[Inst[2]]) - (Inst[5] || Stk[Inst[3]]);
				} else if (Enum == 14) { // MUL
					let Stk = Stack;

					Stk[Inst[1]]	= (Inst[4] || Stk[Inst[2]]) * (Inst[5] || Stk[Inst[3]]);
				} else if (Enum == 15) { // DIV
					let Stk = Stack;

					Stk[Inst[1]]	= (Inst[4] || Stk[Inst[2]]) / (Inst[5] || Stk[Inst[3]]);
				} else if (Enum == 16) { // MOD
					let Stk = Stack;

					Stk[Inst[1]]	= (Inst[4] || Stk[Inst[2]]) % (Inst[5] || Stk[Inst[3]]);
				} else if (Enum == 17) { // POW
					let Stk = Stack;

					Stk[Inst[1]] = Math.pow((Inst[4] || Stk[Inst[2]]), (Inst[5] || Stk[Inst[3]]));
				} else if (Enum == 18) { // UNM
					Stack[Inst[1]]	= -Stack[Inst[2]];
				} else if (Enum == 19) { // NOT
					Stack[Inst[1]] = !Stack[Inst[2]];
				} else if (Enum == 20) { // LEN
					Stack[Inst[1]] = Stack[Inst[2]].length;
				} else if (Enum == 21) { // CONCAT
					let Stk	= Stack;
					let B	= Inst[2];
					let K 	= Stk[B];

                    /*
                        for Idx = B + 1, Inst[3] do
                            K = K .. Stk[Idx];
                        end;
                    */

                    let Loop = B + 1

                    for (let Idx = Loop; Loop <= Inst[3]; Idx++) { 
                        K + Stk[Idx];
                    }

					Stack[Inst[1]]	= K;
				} else if (Enum == 22) { // JMP
					InstrPoint = InstrPoint + Inst[2];
				} else if (Enum == 23) { // EQ
					let Stk = Stack;
					let B = Inst[4] || Stk[Inst[2]];
					let C = Inst[5] || Stk[Inst[3]];
					
					if ((B == C) != Inst[1]) {
						InstrPoint++;
					};
				} else if (Enum == 24) { // LT
					let Stk = Stack;
					let B = Inst[4] || Stk[Inst[2]];
					let C = Inst[5] || Stk[Inst[3]];
					
					if ((B < C) != Inst[1]) {
						InstrPoint++;
					};
				} else if (Enum == 25) { // LE
					let Stk = Stack;
					let B = Inst[4] || Stk[Inst[2]];
					let C = Inst[5] || Stk[Inst[3]];

					if ((B <= C) != Inst[1]) {
						InstrPoint++;
					};
				} else if (Enum == 26) { // TEST
				    if (Inst[3]) { 
                        if (Stack[Inst[1]]) {
                            InstrPoint++;
                        }
				    } else if (Stack[Inst[1]]) {} else {
				        InstrPoint++;
				    }
				} else if (Enum == 27) { // TESTSET
					let B = Stack[Inst[2]];

				    if (Inst[3]) { 
						if (B) {
					    	InstrPoint++;
                        } else { 
					    	Stack[Inst[1]] = B
						}
				    } else if (B) {
				    	Stack[Inst[1]] = B
                    } else {
				    	InstrPoint++;
				    }
				} else if (Enum == 28) { // CALL
					let A	= Inst[1];
					let B	= Inst[2];
					let C	= Inst[3];
					let Stk	= Stack;
					let Args, Results;
					let Limit, Edx;

					Args	= {};

					if (B != 1) {
						if (B != 0) {
							Limit = A + B - 1;
                        } else {
							Limit = Top;
						};

						Edx	= 0;

                        /*
                            for Idx = A++, Limit do
                                Edx = Edx + 1;

                                Args[Edx] = Stk[Idx];
                            end;
                        */

                        let Loop = A++

                        for (let Idx = Loop; Loop <= Limit; Idx++) { 
                            Edx++;

							Args[Edx] = Stk[Idx];
                        }

						Limit, Results = _Returns(Stk[A](Args, 1, Limit - A));
                    } else {
						Limit, Results = _Returns(Stk[A]());
					};

					Top = A - 1;

					if (C != 1) {
						if (C != 0) {
							Limit = A + C - 2;
                        } else {
							Limit = Limit + A - 1;
						};

						Edx	= 0;

                        for (let Idx = A; A <= Limit; Idx++) { 
                            Edx++;

							Args[Edx] = Stk[Idx];
                        }
					};
				} else if (Enum == 29) { // TAILCALL
					let A	= Inst[1];
					let B	= Inst[2];
					let Stk	= Stack;
					let Args, Results;
					let Limit;

					Args = {};

					if (B != 1) {
						if (B != 0) {
							Limit = A + B - 1;
                        } else {
							Limit = Top;
						}

                        let Loop = A++

                        for (let Idx = Loop; Loop <= Limit; Idx++) { 
                            Args[Args.length + 1] = Stk[Idx];
                        }

						Results = [
                            Stk[A](unpack(Args, 1, Limit - A))
                        ];
					} else {
						Results = [ 
                            Stk[A]()
                        ];
					};

                    for (Index in Results) {
                        if (Index > Rets) {
							Rets = Index;
						};   
                    }

					return Results, Rets;
				} else if (Enum == 30) { // RETURN
					let A	= Inst[1];
					let B	= Inst[2];
					let Stk	= Stack;
					let Edx, Output;
					let Limit;

					if (B == 1) {
						return;
					} else if (B == 0) {
						Limit	= Top;
                    } else {
						Limit	= A + B - 2;
					};

					Output = {};
					Edx = 0;

                    for (let Idx = A; A <= Limit; Idx++) { 
                        Edx++;

						Output[Edx] = Stk[Idx];
                    }

					return Output, Edx;
				} else if (Enum == 31) { // FORLOOP
					let A	= Inst[1];
					let Stk	= Stack;

					let Step	= Stk[A + 2];
					let Index	= Stk[A] + Step;

					Stk[A]	= Index;

					if (Step > 0) {
						if (Index <= Stk[A++]) {
							InstrPoint = InstrPoint + Inst[2];

							Stk[A + 3] = Index;
						};
					} else {
						if (Index >= Stk[A++]) {
							InstrPoint = InstrPoint + Inst[2];

							Stk[A + 3] = Index;
						}
					}
				} else if (Enum == 32) { // FORPREP
					let A	= Inst[1];
					let Stk	= Stack;

					Stk[A] = Stk[A] - Stk[A + 2];

					InstrPoint = InstrPoint + Inst[2];
				} else if (Enum == 33) { // TFORLOOP
					let A	= Inst[1];
					let C	= Inst[3];
					let Stk	= Stack;

					let Offset	= A + 2;

					let Result	= {
                        [0]: Stk[A](Stk[A++], Stk[A + 2])
                    };

                    for (let Idx = 1; 1 <= C; Idx++) { 
                        Stack[Offset + Idx] = Result[Idx];
                    }

					if (Stk[A + 3] != null) {
						Stk[A + 2]	= Stk[A + 3];
                    } else {
						InstrPoint++;
					};
				} else if (Enum == 34) { // SETLIST
					let A		= Inst[1];
					let B		= Inst[2];
					let C		= Inst[3];
					let Stk	= Stack;

					if (C == 0) {
						InstrPoint++;
						C = Instr[InstrPoint].Value;
					};

					let Offset	= (C - 1) * 50;
					let T		= Stk[A]; // Assuming T is the newly created table.

					if (B == 0) {
						B = Top - A;
					};

                    for (let Idx = 1; 1 <= B; Idx++) { 
                        T[Offset + Idx] = Stk[A + Idx];
                    }
				} else if (Enum == 35) { // CLOSE
					/*let A		= Inst[1];
					let Cls	= {}; // Slight doubts on any issues this may cause

					for Idx = 1, #Lupvals do
						let List = Lupvals[Idx];

						for Idz = 0, #List do
							let Upv	= List[Idz];
							let Stk	= Upv[1];
							let Pos	= Upv[2];

							if (Stk == Stack) and (Pos >= A) {
								Cls[Pos]	= Stk[Pos];
								Upv[1]		= Cls;
							};
						};
					};*/
				} else if (Enum == 36) { // CLOSURE
					/*let NewProto	= Proto[Inst[2]];
					let Stk	= Stack;

					let Indexes;
					let NewUvals;

					if (NewProto.Upvals != 0) {
						Indexes		= {};
						NewUvals	= setmetatable({}, {
								__index = function(_, Key)
									let Val	= Indexes[Key];

									return Val[1][Val[2]];
								},
								__newindex = function(_, Key, Value)
									let Val	= Indexes[Key];

									Val[1][Val[2]]	= Value;
								};
							}
						);

						for Idx = 1, NewProto.Upvals do
							let Mvm	= Instr[InstrPoint];

							if (Mvm.Enum == 0) { // MOVE
								Indexes[Idx - 1] = {Stk, Mvm[2]};
							} else if (Mvm.Enum == 4) { // GETUPVAL
								Indexes[Idx - 1] = {Upvalues, Mvm[2]};
							};

							InstrPoint	= InstrPoint + 1;
						};

						Lupvals[#Lupvals + 1]	= Indexes;
					};

					Stk[Inst[1]]			= Wrap(NewProto, Env, NewUvals);*/
				} else if (Enum == 37) { // VARARG
					/*let A	= Inst[1];
					let B	= Inst[2];
					let Stk, Vars	= Stack, Vararg;

					Top = A - 1;

					for Idx = A, A + (B > 0 and B - 1 || Varargsz) do
						Stk[Idx]	= Vars[Idx - A];
					};*/
				};
            }
        }

        for (let Idx = 1; Idx <= Varargsz; Idx++) {
            if (Idx >= Chunk.Args) {
                Vararg[Idx - Chunk.Args] = Args[Idx + 1];
            } else {
                Stack[Idx] = Args[Idx + 1];
            }
        }

        try { 
            return Loop() 
        } catch(Error) {
            console.log(Error)

            //OnError(Error, InstrPoint - 1); // Didn't get time to test the `-1` honestly, but I assume it works properly
        }
    }

    return Wrapper
}

module.exports = function(Bytecode, Env) { // lua_function LoadBytecode (string BCode, table Env)
    const Buffer = GetMeaning(Bytecode);

    return Wrap(Buffer, Env)
};
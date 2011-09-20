use feature "switch"; 
use POSIX qw (ceil);

# Constant address mode masks
use constant {
    IMMDT   => 0x1,
    ZEROP   => 0x2,
    ZEROX   => 0x4,
    ABSLT   => 0x8,
    ABSLX   => 0x10,
    ABSLY   => 0x20,
    INDRX   => 0x40,
    INDRY   => 0x80,
    INDRC   => 0x100,
    RELTV   => 0x200,
    IMPLD   => 0x400,
    ACMLT   => 0x800
};

%instructions = ( # First part of opcode, pointer to next part, address mode mask.
              ORA => ["000", \%addr_a, IMMDT|ZEROP|ZEROX|ABSLT|ABSLX|ABSLY|INDRX|INDRY],
              AND => ["001", \%addr_a, IMMDT|ZEROP|ZEROX|ABSLT|ABSLX|ABSLY|INDRX|INDRY],
              EOR => ["010", \%addr_a, IMMDT|ZEROP|ZEROX|ABSLT|ABSLX|ABSLY|INDRX|INDRY],
              ADC => ["011", \%addr_a, IMMDT|ZEROP|ZEROX|ABSLT|ABSLX|ABSLY|INDRX|INDRY],
              STA => ["100", \%addr_a, ZEROP|ZEROX|ABSLT|ABSLX|ABSLY|INDRX|INDRY],
              LDA => ["101", \%addr_a, IMMDT|ZEROP|ZEROX|ABSLT|ABSLX|ABSLY|INDRX|INDRY],
              CMP => ["110", \%addr_a, IMMDT|ZEROP|ZEROX|ABSLT|ABSLX|ABSLY|INDRX|INDRY],
              SBC => ["111", \%addr_a, IMMDT|ZEROP|ZEROX|ABSLT|ABSLX|ABSLY|INDRX|INDRY],
              ASL => ["000", \%addr_b, ZEROP|ZEROX|ABSLT|ABSLX|ACMLT],
              ROL => ["001", \%addr_b, ZEROP|ZEROX|ABSLT|ABSLX|ACMLT],
              LSR => ["010", \%addr_b, ZEROP|ZEROX|ABSLT|ABSLX|ACMLT], 
              ROR => ["011", \%addr_b, ZEROP|ZEROX|ABSLT|ABSLX|ACMLT],
              STX => ["100", \%addr_b, ZEROP|ZEROY|ABSLT],
              LDX => ["101", \%addr_b, IMMDT|ZEROP|ZEROY|ABSLT|ABSLY],
              DEC => ["110", \%addr_b, ZEROP|ZEROX|ABSLT|ABSLX],
              INC => ["111", \%addr_b, ZEROP|ZEROX|ABSLT|ABSLX],
              BIT => ["001", \%addr_c, ZEROP|ABSLT],
              STY => ["100", \%addr_c, ZEROP|ZEROY|ABSLT ],
              LDY => ["101", \%addr_c, IMMDT|ZEROP|ZEROX|ABSLT|ABSLX ],
              CPY => ["110", \%addr_c, IMMDT|ZEROP|ABSLT ],
              CPX => ["111", \%addr_c, IMMDT|ZEROP|ABSLT ],
              JMP => ["01", \%addr_d, ABSLT|INDRC ],
              BPL => ["00010000", undef, RELTV],
              BMI => ["00110000", undef, RELTV],
              BVC => ["01010000", undef, RELTV],
              BVS => ["01110000", undef, RELTV],
              BCC => ["10010000", undef, RELTV],
              BCS => ["10110000", undef, RELTV],
              BNE => ["11010000", undef, RELTV],
              BEQ => ["11110000", undef, RELTV],
              BRK => ["00000000", undef, IMPLD],
              JSR => ["00100000", undef, ABSLT],
              RTI => ["01000000", undef, IMPLD],
              RTS => ["01100000", undef, IMPLD],
              PHP => ["00001000", undef, IMPLD],
              PLP => ["00101000", undef, IMPLD],
              PHA => ["01001000", undef, IMPLD],
              PLA => ["01101000", undef, IMPLD],
              DEY => ["10001000", undef, IMPLD],
              TAY => ["10101000", undef, IMPLD],
              INY => ["11001000", undef, IMPLD],
              INX => ["11101000", undef, IMPLD],
              CLC => ["00011000", undef, IMPLD],
              SEC => ["00111000", undef, IMPLD],
              CLI => ["01011000", undef, IMPLD],
              SEI => ["01111000", undef, IMPLD],
              TYA => ["10011000", undef, IMPLD],
              CLV => ["10111000", undef, IMPLD],
              CLD => ["11011000", undef, IMPLD],
              SED => ["11111000", undef, IMPLD],
              TXA => ["10001010", undef, IMPLD],
              TXS => ["10011010", undef, IMPLD],
              TAX => ["10101010", undef, IMPLD],
              TSX => ["10111010", undef, IMPLD],
              DEX => ["11001010", undef, IMPLD],
              NOP => ["11101010", undef, IMPLD]
          );

%addr_a = (
              &INDRX => ["000", \%opcode_a],
              &ZEROP => ["001", \%opcode_a],
              &IMMDT => ["010", \%opcode_a],
              &ABSLT => ["011", \%opcode_a],
              &INDRY => ["100", \%opcode_a],
              &ZEROX => ["101", \%opcode_a],
              &ABSLY => ["110", \%opcode_a],
              &ABSLX => ["111", \%opcode_a]
             );

%addr_b = (
              &IMMDT  => ["000", \%opcode_b],
              &ZEROP  => ["001", \%opcode_b],
              &ACMLT  => ["010", \%opcode_b],
              &ABSLT  => ["011", \%opcode_b],
              &ZEROX  => ["101", \%opcode_b],
              &ABSLX => ["111", \%opcode_b],
             );

%addr_c = (
              &IMMDT  => ["000", \%opcode_c],
              &ZEROP  => ["001", \%opcode_c],
              &ABSLT  => ["011", \%opcode_c],
              &ZEROX  => ["101", \%opcode_c],
              &ABSLX => ["111", \%opcode_c],
             );

%addr_d = (
              &ABSLT  => ["001100", undef],
              &INDRC => ["101100", undef]
          );

# Compute the final field from existing hashes.
# This is in all honesty a superfluous and pointless operation. I've kept it in for now for clarity in separating opcode fields,
# but it may be taken out later for performance considerations.
foreach $mode (keys %addr_a)
{
    $opcode_a{$mode} = ["01", undef];
}

foreach $mode (keys %addr_b)
{
    $opcode_b{$mode} = ["10", undef];
}

foreach $mode (keys %addr_c)
{
    $opcode_c{$mode} = ["00", undef];
}


# Address mode patterns
$hexpat = "[0-9a-fA-F]";
%addr_modes = (
        IMM => [0,"^\\#\\\$".$hexpat."{2}\$"],
        ZRP => [1,"^\\\$".$hexpat."{2}\$"],
        ZPX => [2,"^\\\$".$hexpat."{2}\$", "^X\$"],
        ABS => [3,"^\\\$".$hexpat."{4}\$"],
        ABSX=> [4,"^\\\$".$hexpat."{4}\$", "^X\$"],
        ABSY=> [5,"^\\\$".$hexpat."{4}\$", "^Y\$"],
        INDX=> [6,"^\\(\\\$".$hexpat."{2},X\\)\$"],
        INDY=> [7,"^\\(\\\$".$hexpat."{2}\\)\$", "^Y\$"],
        INDR=> [8,"^\\(\\\$".$hexpat."{4}\\)\$"],
        REL => [9,"^\*[+-](\d|[1-9][0-9]|1[01][0-9]|12[0-7])\$"],
        IMP => [10,"^\$"],
        ACC => [11,"^A"] );



###############################################################################
################################# SUBROUTINES #################################
###############################################################################
# Tokenise the line
sub tokenise 
{
    my $line = shift;
    $tok  = "";
    @chars = (split(//, $line));
	my @tokens;
TOK:while (@chars)
    {
		while(1) {
			$char = shift @chars;
			last if ($char !~ /[\h]/);

		}
		$match = 1;
		$tok = $char;
		while ($match)
		{
			$tokref = undef;
			given ($tok)
			{
				when (/[\n;]|[^\w\h,\+\-\=:\(\)\"\'\!\@\#\$\%\^\&\*\<\>\.\/\\\?]/)	{ return @tokens }
				when (/,/) { $tokref = ["COMMA", undef] }
				when (/\+/) { $tokref = ["PLUS", "+"] }
                when (/\*/) { $tokref = ["SPLAT", undef] }
				when (/\-/) { $tokref = ["MINUS", "-"] }
				when (/:/)  { $tokref = ["COLON", undef] }
				when (/\=/) { $tokref = ["EQ", undef] }
				when (/\(/) { $tokref = ["LBRACE", undef] }
				when (/\)/) { $tokref = ["RBRACE", undef] }
				when (/\"/) 
				{
					$tok = "";
					$char = shift @chars;
					QUOT: {
						while ($char !~ /\\|\"|\n/) 
						{
							$tok .= $char;
							$char = shift @chars;
						}
						given ($char)
						{
							when (/\\/) 
							{
								$char = shift @chars;
								$tok .= $char;
								$char = shift @chars;
								redo QUOT;
							}
							when (/\"/) { $tokref = ["QUOTEDSTRING", $tok] }
							default { return }
						}
					}

				}
				when (/[\h\t]/) { return  } # unmatched token
				when (/^[a-zA-Z_]$/) 
				{ 
					# Look ahead until we either find an opcode, label or nothing.
					while (($char = shift @chars) =~ /[a-zA-Z0-9_]/) { $tok .= $char }
					given ($tok)
					{
						when (exists($instructions{uc($_)})) { $tokref = ["OPCODE", uc($_)] }
						when (/^[XYA]$/i) { $tokref = ["REG", $_] }
						default  { $tokref = ["LABEL", $_] }
					}
					unshift @chars, split(//, $char);
				}
				when (/#/)	
				{ 
					$char = shift @chars;
					$tok .= $char;
					given ($char)
					{
						when (/\$/)
						{
							while (($char = shift @chars) =~ /$hexpat/) { $tok .= $char }
							$tokref = ["IMM", hex $tok];
						}
						when (/\d/)
						{
							while (($char = shift @chars) =~ /\d/) { $tok .= $char }
							$tokref = ["IMM", $tok];
						}
						when (/L/i)
						{
                            if (($char = shift @chars) =~ /O/i) { $tokref = ["LO", undef] }
						}
                        when (/H/i)
                        {
                            if (($char = shift @chars) =~ /I/i) { $tokref = ["HI", undef] }
                        }
						default { return }
					}
					unshift @chars, split(//, $char);
				}
				when (/\$/)	
				{
					$char = shift @chars;
					given ($char)
					{
						when (/$hexpat/)
						{
							$tok = $char;
							while (($char = shift @chars) =~ /$hexpat/) { $tok .= $char }
							$tokref = ["HEX", hex $tok];
						}
						default { return }
					}
					unshift @chars, split(//, $char);
				}
				when (/^\d|[1-9]\d|1[01]\d|12[0-7]$/) { $tokref = ["INT", $tok] } # Integers
				when (/\./)	
				{ 
					$tok = "";
					while (($char = shift @chars) =~ /[a-zA-Z]/) { $tok .= $char }
					$tokref = ["DEF", $tok]; 	# definition macros
					unshift @chars, split(//, $char);
				}
				default { $tok .= shift @chars }											# anything else
			}
			if (defined($tokref)) 
			{
				push @tokens, $tokref;
				$tok = "";
				$match = 0;
			}
		}
	}
}


# Pass 1 simply scans for labels and pops the PC value into the symbol table for pass 2.

sub pass_one
{
    my $line = shift;
    my $sentence = "";
    my $bytes = 0;
    for $token (@{$line}) 
    {
        given ($token->[0])
        {
            when (/HEX/) { $bytes += $token->[1] >> 8 ? 2 : 1 }
            when (/IMM/) { $bytes++ }
            when (/INT/) { $bytes++ }
            when (/OPCODE/) { $bytes++ }
            when (/DEF/) # Un fsck this code later. it's a bit smelly.
            {
                given ($token->[1])
                {
                    when (/org/) { $ORG = hex $token->[1] }
                }
            }
            when (/QUOTEDSTRING/) { $bytes += length($token->[1]) }
            when (/LABEL (?!EQ|COLON)/) 
            { $bytes++ }
        }
        $sentence .= "$token->[0] " 
    }
    given ($sentence) 
    {
        when (/^LABEL (EQ|COLON)/) 
        { 
            if (!exists $symbol_table{$line->[0][1]}) { $symbol_table{$line->[0][1]} = $PC }
            else { return }
            shift @{$line};
            shift @{$line};
        }
    }
    $PC += $bytes;
    return $line;
}

sub pass_two
{
    my $line = shift;
    my $sentence = "";
    my $instruction = undef;
    my @args;
    my $object;
    for $token (@{$line}) { $sentence .= "$token->[0] " }
    # At the other side of this given block, we should have an instruction byte in $instruction, and any args
    # in $arg, as a binary string representation of its value.
    given ($sentence)
    {
        ## First, the addressing modes ##
        when (/^OPCODE $/) { $instruction = gen_code(IMPLD, $instructions{$line->[0][1]}) }
        when (/^OPCODE REG $/)                                  # Accumulator
        {
            if ($line->[1][1] =~ /A/i) { $instruction = gen_code(ACMLT, $instructions{$line->[0][1]}) }
            else { return }
        }
        when (/^OPCODE IMM $|(LO|HI(?= LABEL $))/)               # Immediate
        {
            $instruction = gen_code(IMMDT, $instructions{$line->[0][1]});
            given ($line->[1][0]) 
            {
                # God help me for this...
                when(/LO/)  { push @args, $ORG + ($symbol_table{$line->[2][1]} & 0xff00) }
                when(/HI/)  { push @args, $ORG + ($symbol_table{$line->[2][1]} >> 8) }
                when(/IMM/) { push @args, $line->[2][1] }
            }
        }
        when (/^OPCODE (HEX|LABEL) $/)                          # Relative(labelled), zero page, absolute
        {
            # If this is an instruction that uses relative, use relative. otherwise, see if zero page or abs fits
            if ($instructions{$line->[0][1]}->[2] & RELTV) 
            { 
                $instruction = gen_code(RELTV, $instructions{$line->[0][1]});
                if ($line->[1][0] =~ /HEX/) { return }
                else { push @args, $ORG + $symbol_table{$line->[1][1]} } # This needs to be something like $ORG + label_addr - PC
            }
            else
            {
                if ($line->[1][0] =~ /HEX/)
                {

                    if ($line->[1][1] >> 8) { $instruction = gen_code(ABSLT, $instructions{$line->[0][1]}) }
                    else { $instruction = gen_code(ZEROP, $instructions{$line->[0][1]}) }
                    push @args, hex $line->[1][1];
                }
                else 
                {
                    if (($line->[0][1] == "JMP") || ($symbol_table{$line->[1][1]} >> 8))
                    { $instruction = gen_code(ABSLT, $instructions{$line->[0][1]}) }
                    else { $instruction = gen_code(ZEROP, $instructions{$line->[0][1]}) }
                    push @args, $ORG + $symbol_table{$line->[1][1]};
                }
            }
        }

        when (/^OPCODE (HEX|LABEL) COMMA REG $/) # Indirect indexed (You are a moron. zp/Abs, Reg.)
        { 
            push @args, $line->[1][0] =~ /HEX/ ? hex($line->[1][1]) : $symbol_table{$line->[1][1]};
        }                
        when (/^OPCODE SPLAT (PLUS|MINUS) INT $/)               # Relative immediate
        {
            $instruction = gen_code(RELTV, $instructions{$line->[0][1]});
            push @args, int($line->[2][1] . $line->[3][1]);
        }
        when (/^OPCODE LBRACE (HEX|LABEL) RBRACE $/) { }            # Indirect
        when (/^OPCODE LBRACE (HEX|LABEL) COMMA REG RBRACE $/) { }  # Indexed indirect
        when (/^OPCODE LBRACE (HEX|LABEL) RBRACE COMMA REG $/)  # Indirect indexed
        {
            if ($line->[2][0] =~ /HEX/) { $arg = hex $line->[2][1] }
            else { push @args, $symbol_table{$line->[2][1]} }
            given ($line->[5][1])
            {
                when (/X/) { $instruction = gen_code(INDRX, $instructions{$line->[0][1]}) }
                when (/Y/) { $instruction = gen_code(INDRY, $instructions{$line->[0][1]}) }
            }
        }
        ## Now, other cruft ##

        when (/DEF HEX (COMMA HEX)*/) 
	{
	    foreach $val (@{$line})
	    {
		if ($val->[0] =~ /HEX/) { push @args, $val->[1] }
	    }
	}
        when (/^DEF QUOTEDSTRING$/) { }                             # String literals
        when (/^(HEX|INT) $/) { push @args, $line->[0][1] }                                    # Constants, labels were removed in pass 1
        default { }                                                 # anything else is an error.
    }
    # Now, work out the args and so on.
    if ($instruction) { $object = pack("B8", $instruction) }
    # Walk the args array, checking type and casting as necessary. append char representations of each byte onto $object
    foreach $arg (@args)
    {
	if ($arg > 0xFF) # multibyte.
	{
	    # The comma separated lists are pushed as bytes or words. So the size of $arg will be at most 16 bits. So we append it one byte at a time using mask and shift
	    $object .= chr($arg & 0xFF00 >> 8);
	    $object .= chr($arg >> 8);
	}
	else { $object .= chr $arg }
    }
    # $object should now contain a string representing the object code for this line.
    return $object;
}



# as-yet unwritten gen_code sub goes WHERE!?
# gen_code takes 2 arguments:
# Addressing mode
# An array containing the instruction opcode, a reference to the relevant address mode table, and the valid address modes mask.
# This should come from the %instructions hash.
# Returns an integer value representing the 8 bit opcode.

sub gen_code
{ 
    #This should work...
    my $mode = shift;
    $instr_vec = shift;
    # Validate the address mode
    ($opcode, $ref, $mask) = @{$instr_vec}[0,1,2];
    $valid = $mode & $mask;
    if ($valid)
    {
	$final_opcode = $opcode;
	# We're in business. check if $ref is defined. if so, wander down the path until we have all the bits we need.
	if (defined ($ref)) 
	{
	    do 
	    { 
		$final_opcode .= $$ref{$mode}->[0]; 
		$ref = $$ref{$mode}->[1]; 
	    } while (defined($ref))
	}
    }
    else { $final_opcode = "YAGSHIMASH $mode $mask $valid \n" }
    return $final_opcode;
}

$PC = 0;
$ORG = 0;
#$\ = $/;
while (<>)
{
    $line++;
    chomp;
    $source_listing{$line} = $_;
    @tokens = tokenise($_);
    if (scalar @tokens) { $raw_code{$line} = [@tokens] }
    @tokens = [];
}

# First pass - resolve label declarations and add them to the symbol table.
foreach $token_ref ( sort {$a <=> $b} keys %raw_code )
{ 
    $raw_code{$token_ref} = pass_one($raw_code{$token_ref}); 
    $pc_line{$token_ref} = $PC;
    if ($raw_code{$token_ref}->[0][0]!~/^$/) { $pass1_code{$token_ref} = $raw_code{$token_ref} }
}


# Second pass - Code generation
foreach $token_ref (sort {$a <=> $b} keys %pass1_code) 
{ 
    $object_code{$token_ref} = pass_two($pass1_code{$token_ref});
}


$PC = 0;
foreach $source_line (sort {$a <=> $b} keys %source_listing) 
{
    $src_field = "";
    $PC = exists $pc_line{$source_line} ? $pc_line{$source_line} : $PC;
    if (exists $raw_code{$source_line})
    {
	foreach $object ($object_code{$source_line})
	{
	    @objects = unpack("C*", $object);
	    foreach $obj (@objects) { $src_field .= sprintf "%02X ", $obj }
	}
    }
    printf("%04X || %04X: %-24s | %s\n", $source_line, $PC, $src_field, $source_listing{$source_line});
}

#for $symbol (keys %symbol_table) { print "$symbol: $symbol_table{$symbol} \n"}

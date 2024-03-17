----------------------------------------------------------------------------------
-- Author: Tarek ELDEEB
----------------------------------------------------------------------------------
  LIBRARY ieee;
  USE ieee.std_logic_1164.ALL;
  USE ieee.numeric_std.ALL;

  library matrix_mul_v1_00_a;
  use matrix_mul_v1_00_a.ai_util_pkg.all;
  
  --------------------------------------------------------------
  -- TC  Opcode         Description                  Status      Octave Code
  --
  -- #1: OPR_MM   Matrix Multiplication 4x4 * 4x4:     OK     a=reshape([0:15],4,4); b=reshape([15:-1:0],4,4); c=a*b'; bb=a*c'
  -- #2: OPR_MM   Matrix Multiplication 4x16 * 16x8:   OK     a=reshape([0:4*16-1],[4,16]); b=reshape([0:16*8-1],[8,16]); a*b'

  -- #4: OPR_TRP  Transpose( 4x4 )                     OK     a=reshape([0:15],4,4); c=a'													
  -- #5: OPR_RTRP Transpose + Reshape 4x6              OK     a=reshape([0:23],4,6); c=reshape(a,6,4)'
  -- #6: OPR_TRP  Transpose (4x6)                      OK     a=reshape([0:15],4,4); c=a'													
  -- #7: OPR_MM   Matrix Multiplication 3x3 * 3x4      OK     a=reshape([0:8],3,3); b=reshape([0:11],4,3); c=a*b'
  -- #8: OPR_MMT  Matrix Multiplication 3x3 * 3x4      OK     a=reshape([0:8],3,3); b=reshape([0:11],4,3); c=(a*b')'
  -- #9: OPR_MM   Matrix Multiplication 4x5 * 5x7      OK     a=reshape([0:19],4,5); b=reshape([0:34],7,5); c=a*b'
  -- #10: OPR_CV  Conv2D(3x3 Kernel, 4x4, VALID)      NOK    k=[0,1,0;1,2,1;0,1,0]; b=reshape([1:16],[4,4]); conv2(b,k,shape='valid')													
  --
  -- TODO/Known Issues
  -- * OCR/OCC are implemented only for burst controller
  --------------------------------------------------------------
  ENTITY matmul_tb IS
    Generic ( 
		n 					: integer := 4;   -- Tested values: 2,[3],4
		k              : integer := 3;   -- Conv Kernel size KxK
		rnd_mem_delay	: boolean := not true
	 );
	 Port (
		test_case_cnf	: in integer := 0;   -- Allowed values: 1-9, 0 for all
		tb_running_o   : out std_logic
	 );
  END matmul_tb;

  ARCHITECTURE behavior OF matmul_tb IS 

  constant Clock_period       : time := 10 ns;
  type t_expected1 is array(0 to 15) of STD_LOGIC_VECTOR(width-1 downto 0);
  constant expected2: t_expected1:=(
                       std_logic_vector(to_unsigned(136, width)),
                       std_logic_vector(to_unsigned(172, width)),
                       std_logic_vector(to_unsigned(112, width)),
                       std_logic_vector(to_unsigned(144, width)),
                       std_logic_vector(to_unsigned(88, width)),
                       std_logic_vector(to_unsigned(116, width)),
                       std_logic_vector(to_unsigned(64, width)),
                       std_logic_vector(to_unsigned(88, width)),
							  std_logic_vector(to_unsigned(208, width)),
                       std_logic_vector(to_unsigned(244, width)),
                       std_logic_vector(to_unsigned(176, width)),
                       std_logic_vector(to_unsigned(208, width)),
							  std_logic_vector(to_unsigned(144, width)),
                       std_logic_vector(to_unsigned(172, width)),
                       std_logic_vector(to_unsigned(112, width)),
                       std_logic_vector(to_unsigned(136, width))
							  );  
  constant expected4: t_expected1:=(
                       std_logic_vector(to_unsigned(136, width)),
                       std_logic_vector(to_unsigned(172, width)),
							  std_logic_vector(to_unsigned(208, width)),
                       std_logic_vector(to_unsigned(244, width)),
                       std_logic_vector(to_unsigned(112, width)),
                       std_logic_vector(to_unsigned(144, width)),
                       std_logic_vector(to_unsigned(176, width)),
                       std_logic_vector(to_unsigned(208, width)),
                       std_logic_vector(to_unsigned(88, width)),
                       std_logic_vector(to_unsigned(116, width)),
							  std_logic_vector(to_unsigned(144, width)),
                       std_logic_vector(to_unsigned(172, width)),
                       std_logic_vector(to_unsigned(64, width)),
                       std_logic_vector(to_unsigned(88, width)),
                       std_logic_vector(to_unsigned(112, width)),
                       std_logic_vector(to_unsigned(136, width))
							  ); 
							  
  SIGNAL clk_del		   : STD_LOGIC := '0';
  signal tb_running,clk,rst	: std_logic:='1';

  signal		  A_Address : STD_LOGIC_VECTOR(awidth-1 downto 0);
  signal		  B_Address : STD_LOGIC_VECTOR(awidth-1 downto 0);
  signal		  C_Address : STD_LOGIC_VECTOR(awidth-1 downto 0);
  signal		  A_outerSz : unsigned(15 downto 0);
  signal		  A_innerSz : unsigned(15 downto 0);
  signal		  B_outerSz : unsigned(15 downto 0);	
  signal      Command 	: STD_LOGIC_VECTOR(15 downto 0);
  signal		  Status    : STD_LOGIC_VECTOR(15 downto 0);
  signal		  interrupt : STD_LOGIC;
  signal		  IA_Address: STD_LOGIC_VECTOR(awidth-1 downto 0);
  signal		  IA_Burst  : STD_LOGIC_VECTOR(15 downto 0);
  signal		  IA_Valid  : STD_LOGIC;
  signal		  IA_Data   : STD_LOGIC_VECTOR(width-1 downto 0);
  signal		  IB_Address: STD_LOGIC_VECTOR(awidth-1 downto 0);
  signal		  IB_Burst  : STD_LOGIC_VECTOR(15 downto 0);
  signal		  IB_Valid  : STD_LOGIC;
  signal		  IB_Data   : STD_LOGIC_VECTOR(width-1 downto 0);
  signal		  OC_Address: STD_LOGIC_VECTOR(awidth-1 downto 0);
  signal		  OC_Valid  : STD_LOGIC;
  signal		  OC_Data   : STD_LOGIC_VECTOR(width-1 downto 0);
  
  signal 	  rCommand	: t_command:=(reserved=>(others=>'0'),operation=>(others=>'0'),clear_err=>(others=>'0'),run=>"0");
  signal 	  rStatus	: t_status;

  	FUNCTION fill_mem (test_case: IN integer) RETURN mem_init_t IS
        VARIABLE mem : mem_init_t := (OTHERS => (OTHERS => '1'));
    BEGIN
        Case test_case is
			 when 1 =>
				for i in 0 to 15 loop
					mem(4+i)	:= std_logic_vector(to_unsigned(i,32));	
					mem(35-i)	:= std_logic_vector(to_unsigned(i,32));	
				end loop;
			 when 2 =>
				for i in 0 to 4*16-1 loop --A
					mem(4+i)	:= std_logic_vector(to_unsigned(i,32));	
				end loop;
				for i in 0 to 16*8-1 loop --B
					mem(4+4*16+i)	:= std_logic_vector(to_unsigned(i,32));	
				end loop;
			 when 3 =>
				--Kernel
				mem(4)	:= std_logic_vector(to_unsigned(0,32));	
				mem(5)	:= std_logic_vector(to_unsigned(1,32));	
				mem(6)	:= std_logic_vector(to_unsigned(0,32));	
				mem(7)	:= std_logic_vector(to_unsigned(1,32));	
				mem(8)	:= std_logic_vector(to_unsigned(2,32));	
				mem(9)	:= std_logic_vector(to_unsigned(1,32));	
				mem(10)	:= std_logic_vector(to_unsigned(0,32));	
				mem(11)	:= std_logic_vector(to_unsigned(1,32));	
				mem(12)	:= std_logic_vector(to_unsigned(0,32));	
				for i in 1 to 16 loop --B
					mem(15+i)	:= std_logic_vector(to_unsigned(i,32));	
				end loop;	
			 when 4 =>
				for i in 0 to 15 loop --To Transpose
					mem(4+i)	:= std_logic_vector(to_unsigned(i,32));	
				end loop;				
			 when 5 =>
				for i in 0 to 23 loop --To Reshape + Transpose
					mem(4+i)	:= std_logic_vector(to_unsigned(i,32));	
				end loop;
			 when 6 =>
				for i in 0 to 23 loop --To Transpose
					mem(4+i)	:= std_logic_vector(to_unsigned(i,32));	
				end loop;
			 when 7 =>
				for i in 0 to 8 loop
					mem(4+i)	:= std_logic_vector(to_unsigned(i,32));	
				end loop;
				for i in 0 to 11 loop
					mem(16+i)	:= std_logic_vector(to_unsigned(i,32));	
				end loop;				
			 when 8 =>
				for i in 0 to 8 loop
					mem(4+i)	:= std_logic_vector(to_unsigned(i,32));	
				end loop;
				for i in 0 to 11 loop
					mem(16+i)	:= std_logic_vector(to_unsigned(i,32));	
				end loop;
			 when 9 =>
				for i in 0 to 19 loop
					mem(4+i)	:= std_logic_vector(to_unsigned(i,32));	
				end loop;
				for i in 0 to 34 loop
					mem(24+i)	:= std_logic_vector(to_unsigned(i,32));	
				end loop;
			 when 10 =>
				--Kernel
				mem(4)	:= std_logic_vector(to_unsigned(0,32));	
				mem(5)	:= std_logic_vector(to_unsigned(1,32));	
				mem(6)	:= std_logic_vector(to_unsigned(0,32));	
				mem(7)	:= std_logic_vector(to_unsigned(1,32));	
				mem(8)	:= std_logic_vector(to_unsigned(2,32));	
				mem(9)	:= std_logic_vector(to_unsigned(1,32));	
				mem(10)	:= std_logic_vector(to_unsigned(0,32));	
				mem(11)	:= std_logic_vector(to_unsigned(1,32));	
				mem(12)	:= std_logic_vector(to_unsigned(0,32));	
				for i in 1 to 16 loop --B
					mem(15+i)	:= std_logic_vector(to_unsigned(i,32));	
				end loop;					
			when others => 
			   Null;
		  end case;
        RETURN mem;
    END FUNCTION fill_mem;	

  	FUNCTION get_test_case (test_case: IN integer) RETURN t_testcase IS
        VARIABLE tc : t_testcase;
    BEGIN
        Case test_case is
			 when 1 =>
			 	 tc.mem_init  := fill_mem(test_case);
				 tc.mem_size  := 64;
				 tc.configur  := ( A_Address=>X"00000004",
													B_Address=>X"00000014",
													C_Address=>X"00000024",
													A_outerSz=>to_unsigned(4,16), 
													A_innerSz=>to_unsigned(4,16), 
													B_outerSz=> to_unsigned(4,16));
			 when 2 =>
			 	 tc.mem_init  := fill_mem(test_case);
				 tc.mem_size  := 256;
				 tc.configur  := ( A_Address=>X"00000004",
													B_Address=>X"00000044",
													C_Address=>X"000000C4",
													A_outerSz=>to_unsigned(4,16), 
													A_innerSz=>to_unsigned(16,16), 
													B_outerSz=>to_unsigned(8,16));
			 when 3 =>
			 	 tc.mem_init  := fill_mem(test_case);
				 tc.mem_size  := 64;
				 tc.configur  := ( A_Address=>X"00000004",
													B_Address=>X"00000010",
													C_Address=>X"00000024",
													A_outerSz=>to_unsigned(3,16), --K
													A_innerSz=>to_unsigned(4,16), 
													B_outerSz=>to_unsigned(4,16));
			 when 4 =>
			 	 tc.mem_init  := fill_mem(test_case);
				 tc.mem_size  := 64;
				 tc.configur  := ( A_Address=>X"00000004",
													B_Address=>X"00000000",
													C_Address=>X"00000014",
													A_outerSz=>to_unsigned(4,16), 
													A_innerSz=>to_unsigned(4,16), 
													B_outerSz=>to_unsigned(0,16));
			 when 5 =>
			 	 tc.mem_init  := fill_mem(test_case);
				 tc.mem_size  := 64;
				 tc.configur  := ( A_Address=>X"00000004",
													B_Address=>X"00000000",
													C_Address=>X"0000001C",
													A_outerSz=>to_unsigned(4,16), 
													A_innerSz=>to_unsigned(6,16), 
													B_outerSz=>to_unsigned(0,16));
			 when 6 =>
			 	 tc.mem_init  := fill_mem(test_case);
				 tc.mem_size  := 64;
				 tc.configur  := ( A_Address=>X"00000004",
													B_Address=>X"00000000",
													C_Address=>X"0000001C",
													A_outerSz=>to_unsigned(4,16), 
													A_innerSz=>to_unsigned(6,16), 
													B_outerSz=>to_unsigned(0,16));
			 when 7 =>
			 	 tc.mem_init  := fill_mem(test_case);
				 tc.mem_size  := 64;
				 tc.configur  := ( A_Address=>X"00000004",
													B_Address=>X"00000010",
													C_Address=>X"00000024",
													A_outerSz=>to_unsigned(3,16), 
													A_innerSz=>to_unsigned(3,16), 
													B_outerSz=> to_unsigned(4,16)); 
			 when 8 =>
			 	 tc.mem_init  := fill_mem(test_case);
				 tc.mem_size  := 64;
				 tc.configur  := ( A_Address=>X"00000004",
													B_Address=>X"00000010",
													C_Address=>X"00000024",
													A_outerSz=>to_unsigned(3,16), 
													A_innerSz=>to_unsigned(3,16), 
													B_outerSz=> to_unsigned(4,16)); 
			 when 9 =>
			 	 tc.mem_init  := fill_mem(test_case);
				 tc.mem_size  := 128;
				 tc.configur  := ( A_Address=>X"00000004",
													B_Address=>X"00000018",
													C_Address=>X"00000040",
													A_outerSz=>to_unsigned(4,16), 
													A_innerSz=>to_unsigned(5,16), 
													B_outerSz=> to_unsigned(7,16)); 
			 when 10 =>
			 	 tc.mem_init  := fill_mem(test_case);
				 tc.mem_size  := 64;
				 tc.configur  := ( A_Address=>X"00000004",
													B_Address=>X"00000010",
													C_Address=>X"00000024",
													A_outerSz=>to_unsigned(3,16), --K
													A_innerSz=>to_unsigned(4,16), 
													B_outerSz=>to_unsigned(4,16));													
			 when others => Null;
		  end case;
        RETURN tc;
    END FUNCTION get_test_case;

  signal test_case		: integer := max(1, test_case_cnf);	
  constant tc_const		: t_testcase:= get_test_case(test_case);									
  signal tc					: t_testcase:= tc_const;
  
  BEGIN
--  

  Clock_process :process
  begin
    if tb_running = '1' then
		 clk <= '1';
		 wait for Clock_period/2;
		 clk <= '0';
		 wait for Clock_period/2;
	 else
		 wait for Clock_period;
	 end if;
  end process;
 

  --  Test Bench Statements
     tb : PROCESS
     BEGIN

       wait for 10*Clock_period; -- wait until global set/reset completes
		 rst	<= '0';
		 wait for Clock_period;	 
	    report "Starting TC #" & integer'image(test_case);
		 if test_case = 1 then
		 	 rCommand.run			<= "1";
		 	 rCommand.operation	<= OPR_MM;
			 wait for Clock_period;
			 rCommand.run			<= "0";
			 
			 if n = 2 then
				 for i in 0 to 3 loop
					 wait until OC_Valid = '1';
					 for j in 0 to 3 loop
						 assert expected2(4*i+j) = OC_Data
							 report "MATMUL Error Value at Block:" & integer'image(i) & " Index = "& integer'image(j) 
							 & "; expected: "& integer'image(to_integer(signed(expected2(4*i+j)))) 
							 & ", but got:  "& integer'image(to_integer(signed(OC_Data)))
							 severity warning;
						 wait for 1.001*Clock_period;
					 end loop;		
				 end loop;
				 
			  elsif n = 3 then
				  wait for 10*Clock_period;
				  if rStatus.error_size = "1" then
					  rCommand.clear_err	<= "1";
					  wait for Clock_period;
					  rCommand.clear_err	<= "0";		 
				  end if;
				  
			  elsif n = 4 then
				  wait until OC_Valid = '1';
				  for j in 0 to 15 loop
					  assert expected4(j) = OC_Data
						 report "MATMUL Error Value at Index = "& integer'image(j) 
						 & "; expected: "& integer'image(to_integer(signed(expected4(j)))) 
						 & ", but got:  "& integer'image(to_integer(signed(OC_Data)))
						 severity warning;
					  wait for 1.001*Clock_period;
				  end loop;
				  
				  tc.configur.B_Address	<=tc.configur.C_Address;
				  tc.configur.C_Address	<=tc.configur.B_Address;-- B=AxC
				  rCommand.run			<= "1";
				  rCommand.operation	<= OPR_MMT;
				  wait for Clock_period;
				  rCommand.run			<= "0";
				  
				  --TODO: Validate second operation!
				  wait until OC_Valid = '1';
				  wait until OC_Valid = '0';
				  
			  else
				  assert false  report "Wrong Value of N"  severity FAILURE;
			  end if;
		 elsif test_case = 2 then
		    rCommand.run			<= "1";
		 	 rCommand.operation	<= OPR_MM;
			 wait for Clock_period;
			 rCommand.run			<= "0";
		    
			 for i in 0 to 2*(4/n)**2 -1 loop
				 wait until OC_Valid = '1';
				 wait until OC_Valid = '0';
			 end loop;	 
		 elsif test_case = 3 then
		    rCommand.run			<= "1";
		 	 rCommand.operation	<= OPR_EXP;
			 wait for Clock_period;
			 rCommand.run			<= "0";
		    wait for 10*Clock_period;
			 if rStatus.error = "1" then
			   rCommand.clear_err	<= "1";
			   wait for Clock_period;
			   rCommand.clear_err	<= "0";		 
			 end if;
		 elsif test_case = 4 then
		    rCommand.run			<= "1";
		 	 rCommand.operation	<= OPR_TRP;
			 wait for Clock_period;
			 rCommand.run			<= "0";
		    for i in 0 to (4/n)**2 -1 loop
				 wait until OC_Valid = '1';
				 wait until OC_Valid = '0';
			 end loop;			 
		 elsif test_case = 5 then
		    rCommand.run			<= "1";
		 	 rCommand.operation	<= OPR_RTRP;
			 wait for Clock_period;
			 rCommand.run			<= "0";
		    for i in 0 to 6-1 loop
				 wait until OC_Valid = '1';
				 wait until OC_Valid = '0';
			 end loop;
		 elsif test_case = 6 then
		    rCommand.run			<= "1";
		 	 rCommand.operation	<= OPR_TRP;
			 wait for Clock_period;
			 rCommand.run			<= "0";
		    for i in 0 to 6-1 loop
				 wait until OC_Valid = '1';
				 wait until OC_Valid = '0';
			 end loop;
       elsif test_case = 7 then
		 	 rCommand.run			<= "1";
		 	 rCommand.operation	<= OPR_MM;
			 wait for Clock_period;
			 rCommand.run			<= "0";
			 
			 if n = 3 then
				 for i in 0 to 1 loop
					 wait until OC_Valid = '1';
					 for j in 0 to 2 loop
						 wait for 1.001*Clock_period;
					 end loop;		
				 end loop;
			  else
				  assert false  report "TC-7 requires N=3 only"  severity FAILURE;
			  end if;
       elsif test_case = 8 then
		 	 rCommand.run			<= "1";
		 	 rCommand.operation	<= OPR_MMT;
			 wait for Clock_period;
			 rCommand.run			<= "0";
			 
			 if n = 3 then
				 for i in 0 to 1 loop
					 wait until OC_Valid = '1';
					 for j in 0 to 2 loop
						 wait for 1.001*Clock_period;
					 end loop;		
				 end loop;
			  else
				  assert false  report "TC-7 requires N=3 only"  severity FAILURE;
			  end if;				  
       elsif test_case = 9 then
		 	 rCommand.run			<= "1";
		 	 rCommand.operation	<= OPR_MM;
			 wait for Clock_period;
			 rCommand.run			<= "0";			 
			 for i in 0 to 7 loop
				 wait until OC_Valid = '1';
				 for j in 0 to 2 loop
					 wait for 1.001*Clock_period;
				 end loop;		
			 end loop;
		 elsif test_case = 10 then
		    rCommand.run			<= "1";
		 	 rCommand.operation	<= OPR_CV;
			 wait for Clock_period;
			 rCommand.run			<= "0";
		    --for i in 0 to 1 loop
				 wait until OC_Valid = '1';
				 wait until OC_Valid = '0';
			 --end loop;			 
		  else
		    assert false  report "Wrong Value of Test Case"  severity FAILURE;
		 end if;
		 				 
		 wait for 10*Clock_period;	--Wait for extra time to check for post running errors 
		 tb_running <= '0';        --No clock .. exit this process
		 --report "TIP> type:  show value /matmul_tb/mem/ram/ram";
		 
	    rst	<= '1';
		 wait for Clock_period;
		 if test_case_cnf = 0 then
			test_case  <= test_case + 1;
			tc 		  <= get_test_case(test_case + 1);
			tb_running <= '1';
		 else
			wait;
		 end if;
		 
     END PROCESS tb;
  --  End Test Bench 
  
  Command	<= rCommand.reserved & rCommand.operation & rCommand.clear_err & rCommand.run;  
  config_to_vecs(tc.configur, A_Address,B_Address,C_Address,A_outerSz, A_innerSz, B_outerSz);
  rStatus   <= slv_to_t_status(Status);
  clk_del <= clk AND Not(rst);
  tb_running_o	<= tb_running;
  
  -- Component Instantiation
  uut: entity matrix_mul_v1_00_a.matmul
	 Generic Map(
			  n			=> n, 	-- Number of PEs, and implicitly the matrix size of A and B (n*n).
			  width		=> width) -- Data width, assuming signed integers
    Port Map( 
			  clk 		=> clk,
           rst 		=> rst,
			  A_Address => A_Address,
			  B_Address => B_Address,
			  C_Address => C_Address,
			  A_outerSz => std_logic_vector(A_outerSz),
			  A_innerSz => std_logic_vector(A_innerSz),
			  B_outerSz => std_logic_vector(B_outerSz),
           Command 	=> Command,
			  Status    => Status,
			  interrupt => interrupt,
			  IA_Address=> IA_Address,
			  IA_Burst	=> IA_Burst,
			  IA_Valid  => IA_Valid,
			  IA_Data   => IA_Data,
			  IB_Address=> IB_Address,
			  IB_Burst	=> IB_Burst,
			  IB_Valid  => IB_Valid,
			  IB_Data   => IB_Data,
			  OC_Address=> OC_Address,
			  OC_Valid  => OC_Valid,
			  OC_Data   => OC_Data
			  );

mem: entity matrix_mul_v1_00_a.matmul_memory
  	 Generic map (
	   n        => n,
		k			=> k,
	   --size		=> tc_const.mem_size,
		width		=> width,
		init		=> tc_const.mem_init,
		rnd_delay=> rnd_mem_delay)
    Port map( 
	   clk		=> clk_del,
		-- synthesis translate_off
		reinit	=> tc.mem_init,
      -- synthesis translate_on 
		Patchify => rCommand.operation(OPR_BIT_CV), -- Load A as kernel
		PadSame  => rCommand.operation(OPR_BIT_S),  -- Padding = 'Same'? Otherwise 'No padding'/Valid.
		A_Address=> IA_Address,
		A_Burst  => IA_Burst,
		A_Valid  => IA_Valid,
		A_Data   => IA_Data,
		B_Address=> IB_Address,
		B_Burst  => IB_Burst,
		B_Valid  => IB_Valid,
		B_Data   => IB_Data,
		C_Address=> OC_Address,
		C_Valid  => OC_Valid,
		C_Data   => OC_Data
	);
  
  END;

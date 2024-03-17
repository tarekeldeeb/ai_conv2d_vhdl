-- TestBench Template 

  LIBRARY ieee;
  USE ieee.std_logic_1164.ALL;
  USE ieee.numeric_std.ALL;

  ENTITY tb_all IS
	 Generic ( 
		n 					: in integer := 3;   -- Tested values: 2,[3],4
		k              : in integer := 3;   -- Kernel size KxK
		rnd_mem_delay	: in boolean := not true
	 );
  END tb_all;

  ARCHITECTURE behavior OF tb_all IS 

	  -- Component Declaration
	  COMPONENT   matmul_tb IS
		 Generic ( 
			n 					: in integer := 3;   -- Tested values: 2,[3],4
			k              : in integer := 3;   -- Kernel size KxK
			rnd_mem_delay	: in boolean := not true
		 );
		 Port (
			test_case_cnf		: in integer := 9;   -- Allowed values: 1-9
			tb_running_o   : out std_logic
		 );
	  END COMPONENT;

	 SIGNAL tc :  integer:=1;
	 SIGNAL tb_running :  std_logic;
          

  BEGIN

  -- Component Instantiation
          testCases: matmul_tb 
						Generic Map(n,k,rnd_mem_delay)
						PORT MAP(
                  test_case_cnf => tc,
                  tb_running_o => tb_running
          );


  --  Test Bench Statements
     tb : PROCESS
     BEGIN
		  tcs: for i in 1 to 9 loop
		    tc <= i;
			 wait for 100 ns; -- wait until TC starts
			 report "Running ..";
			 wait until tb_running = '0'; 
          report "Done!";			 
		  end loop;

     END PROCESS tb;
  --  End Test Bench 

  END;

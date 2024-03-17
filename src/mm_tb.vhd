-- TestBench Template

  LIBRARY ieee;
  USE ieee.std_logic_1164.ALL;
  USE ieee.numeric_std.ALL;

  library matrix_mul_v1_00_a;
  use matrix_mul_v1_00_a.ai_util_pkg.all;

  ENTITY mm_tb IS
    Generic (
		n 				: integer := 3;
		width			: integer := 16;
		Cout_Rows	: STD_LOGIC := '0';
		Inj_Pauses 	: BOOLEAN	:= true
	 );
  END mm_tb;

	 ----------------------------------------
	 -- Octave code for the same stimulus:
	 ----------------------------------------
	 -- n=3
	 -- a=reshape(n^2:-1:1,[n,n])
	 -- b=reshape(1:n^2,[n,n])'
	 -- Cout=a*b
	 ----------------------------------------

  ARCHITECTURE behavior OF mm_tb IS

  -- Component Declaration
  component MM is
	 Generic (
	   n				: integer := 3;
	   width			: integer := 16);
    Port (
	   clk 			: in  STD_LOGIC;
	   rst 			: in  STD_LOGIC;
	   rst_C			: in  STD_LOGIC;
	   AB_val		: in  STD_LOGIC;
	   A 				: in  STD_LOGIC_VECTOR (width-1 downto 0);
	   B 				: in  STD_LOGIC_VECTOR (width-1 downto 0);
	   Cout_Rows	: in  STD_LOGIC;
		Cout_count  : in  unsigned(ilog2(n)-1 downto 0);
	   C_out 		: out STD_LOGIC_VECTOR (width-1 downto 0);
	   Cout_val 	: out STD_LOGIC);
  end component;

  function gemm_seq(n : integer; i : integer; rows : std_logic) return integer is
    variable result : integer:=0;
	 variable x,y : integer; -- index for row/col
  begin
    if rows = '1' then
		x := i/n;
		y := i mod n;
	 else
		y := i/n;
		x := i mod n;
	 end if;
    for nn in 0 to n-1 loop
		result := result + (n**2-x-nn*n)*(1+y+nn*n);
	 end loop;
    return result;
  end;

    constant Clock_period              : time := 10 ns;
	 SIGNAL RESET_C							: std_logic:='1';
	 SIGNAL ACCUM_C							: std_logic:='0';
	 SIGNAL clk, rst, rst_C					: std_logic;
	 SIGNAL AB_val, C_val 					: std_logic;
	 SIGNAL Cout_count                  : unsigned(ilog2(n)-1 downto 0):=to_unsigned(n,ilog2(n));
	 signal A,B,C 								: std_logic_vector(width-1 downto 0):= (others=>'U');
	 signal tb_running						: std_logic:='1';
	 signal en_Pauses							: boolean:=true;

	 procedure do_MM(
		signal rst_C_in  	: in  STD_LOGIC;
		signal rst, rst_C : out STD_LOGIC;
	   signal AB_val		: inout STD_LOGIC;
	   signal A, B 		: out STD_LOGIC_VECTOR (width-1 downto 0);
		signal Cout_count : out unsigned(ilog2(n)-1 downto 0);
		constant remainder: in boolean
	 ) is
	 begin
		  rst		<= '1';
		  rst_C	<= rst_C_in;
		  AB_val	<= '0';
        wait for 35 ns;
		  rst		<= '0';
		  rst_C	<= '0';
        wait for 15 ns;
		  if remainder = true then
		     Cout_count	<= to_unsigned(n-1,ilog2(n));
		  else
		     Cout_count	<= to_unsigned(n,ilog2(n));		  
		  end if;

		  AB_val	<= '1';

		  for i in 0 to n-1 loop
				B	<= std_logic_vector(to_unsigned(i+1,width));
				wait for Clock_period;
				pauseSignal(AB_val,3*Clock_period,Inj_Pauses AND en_Pauses AND i=1); -- Pause while loading B
		  end loop;

		  for i in 0 to n*(n-1)-1 loop
				B	<= std_logic_vector(to_unsigned(i+n+1,width));
				A	<= std_logic_vector(to_unsigned(n*n-i,width));
				wait for Clock_period;
				pauseSignal(AB_val,3*Clock_period,Inj_Pauses AND en_Pauses AND i=n); -- Pause while loading A+B
		  end loop;
		  
		  A	<= (others=>'U');
		  B	<= (others=>'U');
		  pauseSignal(AB_val,3*Clock_period,Inj_Pauses AND en_Pauses); -- Pause while loading A+B

		  for i in 0 to n-1 loop
				B	<= (others=>'U');
				A	<= std_logic_vector(to_unsigned(n-i,width));
				wait for Clock_period;
				-- TODO: Handle!
				--pauseSignal(AB_val,2*Clock_period,Inj_Pauses AND i=1); -- Pause while loading A
		  end loop;

        AB_val	<= '0';
	  	  A	<= (others=>'U');
	     B	<= (others=>'U');
		  for i in 0 to n*n loop
				wait for Clock_period;
		  end loop;
	 end do_MM;

  BEGIN

  -- Component Instantiation
	 uut: MM GENERIC MAP (n,width)
				PORT MAP(
				  clk 		=> clk,
				  rst 		=> rst,
				  rst_C 		=> rst_C,
				  AB_val		=> AB_val,
				  A 			=> A,
				  B 			=> B,
				  Cout_Rows	=> Cout_Rows,
				  Cout_count=> Cout_count,
				  C_out 		=> C,
				  Cout_val	=> C_val);

  Clock_process :process
  begin
    if tb_running = '1' then
		 clk <= '1';
		 wait for Clock_period/2;
		 clk <= '0';
		 wait for Clock_period/2;
	 else
		 wait;
	 end if;
  end process;

  tb : PROCESS -- Process to stream-in the inputs
     BEGIN
		  do_MM(	RESET_C, rst, rst_C , AB_val, A, B , Cout_count, false );
		  wait for 30 ns;
		  --do_MM(	ACCUM_C, rst, rst_C , AB_val, A, B , Cout_count, false );
		  en_Pauses	<= false;
		  do_MM(	RESET_C, rst, rst_C , AB_val, A, B , Cout_count, false );
		  wait for 30 ns;
		  tb_running	<= '0';
        wait;
     END PROCESS tb;

  checks: PROCESS(clk,rst) -- Process to check output values
    variable cout_index : integer:=0;
	 variable expected   : integer;
	 BEGIN
	   if rst='1' then
		   cout_index := 0;
		elsif rising_edge(clk) and C_val = '1' then
			expected := gemm_seq(n,cout_index,Cout_Rows);
			assert expected = to_integer(signed(C))
				report "GEMM Error Value at Index = "& integer'image(cout_index) & "; expected: "& integer'image(expected) & ", but got:  "& integer'image(to_integer(signed(C)))
				severity warning;
			cout_index := cout_index + 1;
		end if;
    END PROCESS checks;

  END;

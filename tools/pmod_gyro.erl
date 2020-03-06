-module(pmod_gyro).

-behavior(gen_server).

-include("grisp.hrl").
-include("pmod_gyro.hrl").

% API
-export([start_link/2]).
-export([read/0]).

% Callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-define(SPI_MODE, #{cpol => low, cpha => leading}).

%--- API -----------------------------------------------------------------------

% @private
start_link(Slot, Opts) ->
    gen_server:start_link(?MODULE, [Slot, Opts], []).

read() ->
    Dev = grisp_devices:default(?MODULE),
    case gen_server:call(Dev#device.pid, read) of
        {error, Reason} -> error(Reason);
        Result          -> Result
    end.

%--- Callbacks -----------------------------------------------------------------

% @private
init([Slot, Opts]) ->
    verify_device(Slot),
    Res = maps:get(resolution, Opts, 250),
    ResOpt = case Res of
                 250   -> 2#00000000;
                 500   -> 2#00010000;
                 2000  -> 2#00100000; % writeRegisterViaSPI(CTRL_REG4, 0b00110000);
                 _     -> error({invalid_option, Res})
             end,
    %% set the resolution
    <<>> = grisp_spi:send_recv(Slot, ?SPI_MODE, <<?RW_WRITE:1, ?MS_SAME:1, ?CTRL_REG4:6, ResOpt:8>>, 2, 0),
    %% enable the device and axis sensors
    <<>> = grisp_spi:send_recv(Slot, ?SPI_MODE, <<?RW_WRITE:1, ?MS_SAME:1, ?CTRL_REG1:6, 2#00001111:8>>, 2, 0),
    grisp_devices:register(Slot, ?MODULE),
    {ok, #{slot => Slot, unit_degree => (32766 / Res)}}.

% @private
handle_call(read, _From, #{slot := Slot, unit_degree := UnitDeg} = State) ->
    <<X:16/signed-little,
      Y:16/signed-little,
      Z:16/signed-little>> = grisp_spi:send_recv(Slot, ?SPI_MODE,
                                                 <<?RW_READ:1, ?MS_INCR:1, ?OUT_X_L:6>>,
                                                 1, 6),
    {reply, {X / UnitDeg, Y / UnitDeg, Z / UnitDeg}, State};
handle_call(Request, From, _State) ->
    error({unknown_request, Request, From}).

% @private
handle_cast(Request, _State) -> error({unknown_cast, Request}).

% @private
handle_info(Info, _State) -> error({unknown_info, Info}).

% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @private
terminate(_Reason, _State) -> ok.

%--- Internal ------------------------------------------------------------------

verify_device(Slot) ->
    case grisp_spi:send_recv(Slot, ?SPI_MODE, <<?RW_READ:1, ?MS_SAME:1, ?WHO_AM_I:6>>, 1, 1) of
        <<?DEVID>> -> ok;
        Other      -> error({device_mismatch, {who_am_i, Other}})
    end.

% (achlys@my_grisp_board_2)34> grisp_spi:send_recv(spi1, #{cpol => low, cpha => leading}, <<1:1, 0:1, 15:6>>, 1, 1).
% <<"Ó">>
% (achlys@my_grisp_board_2)35> grisp_bitmap:pp(<<"Ó">>,nib).
% 1101 0011 %% (DEVID was 1101 0011 = response)
% ok

%% @spec send_recv(spi_slot(), spi_mode(), binary(), integer(), integer()) ->
%% Response::binary()
% send_recv(Slot, Mode, Req, Skip, Pad)

% maps:find(who_am_i, pmod_nav:registers(acc)).
% {ok,{15,read,1,raw}}


% CS = LOW
% SPI_MODE0 = CPOL_LOW bor CPHA_LEADING = 0 bor 0 = 0.
% RW_READ = 1
% MS_SAME = 0
% grisp_bitmap:pp(<<16#0F>>,#{display => bin}).
% 00001111
% ok
% <<RW_READ:1, MS_SAME:1, WHO_AM_I:6>> -------> 1100 1111 = 16#CF ? 
% -> % L3G4200D -----------> PmodGYRO
% -> WHO_AM_I: 1101 0011 <--> DEVID ? 


% /** Sets up control registers of PMod GYRO ready for use.
%  *  
%  * Parameters:
%  * scale - determines recording rate (can use 250, 500 or 2000 deg/sec) 
%  */
% void setupGyro(int scale){
%   /* Enable x, y, z and turn off power down */
%   writeRegisterViaSPI(CTRL_REG1, 0b00001111);
 
%   /* CTRL_REG4 controls the full-scale range (amongst other things) */
%   if(scale == 250){
%     writeRegisterViaSPI(CTRL_REG4, 0b00000000);
%   }else if(scale == 500){
%     writeRegisterViaSPI(CTRL_REG4, 0b00010000);
%   }else{
%     writeRegisterViaSPI(CTRL_REG4, 0b00110000);
%   }
% }

% https://www.st.com/content/ccc/resource/sales_and_marketing/presentation/product_presentation/bf/83/4a/ba/83/e2/49/f2/l3gd20_l3g4200d_marketing_pres.pdf/files/l3gd20_l3g4200d_marketing_pres.pdf/jcr:content/translations/en.l3gd20_l3g4200d_marketing_pres.pdf
% Register differences
% - The L3G4200D and L3GD20 present the same register structure
% - The L3G4200D and L3GD20 have different WHO_AM_I and device slave addresses (I2C)
%    L3GD20
% - WHO_AM_I: 1101 0100
% - Slave address: 1101 01xx
% L3G4200D -----------> PmodGYRO
% - WHO_AM_I: 1101 0011 -------- 16#D3 = 211 decimal
% - Slave address: 1101 00xx


% From pmod_gyro.hrl : 

% -define(WHO_AM_I,         16#0F).
% -define(CTRL_REG1,        16#20).
% -define(CTRL_REG4,        16#23).
% -define(OUT_TEMP,         16#26).
% -define(OUT_X_L,          16#28).

%--- Bit Descriptions ----------------------------------------------------------

% WHO_AM_I
% -define(DEVID,            2#11010011).

% The on-board chip has two possible slave address in the form of 110100x where x
% is the voltage state of the Master-In-Slave-Out (MISO) pin on the SPI header.
% After the slave address and the read or write bit has been transmitted and the
% message was acknowledged, a 7-bit register address can then be transmitted. The
% most significant bit (the first bit of the 8-bit of the transfer) indicates if
% multiple bytes of information are to be transferred.
% -define(DEVID,            2#11010001).

% https://www.hackster.io/56584/using-the-pmod-gyro-with-arduino-uno-da7b29
% * Test of the Pmod
% *
% *************************************************************************
% * Description: Pmod_GYRO
% * The 3 components X, Y, and Z of the gyroscope
% * are displayed in the serial monitor
% *
% * Material
% * 1. Arduino Uno
% * 2. Pmod GYRO
% *
% ************************************************************************/

% // Déclaration des adresses du module
% #define L3G4200D_Adresse 0x69 // adress of L3G4200D ---------> 2# 0110 1001
% #define CTRL_REG1 0x20
% #define CTRL_REG2 0x21
% #define CTRL_REG3 0x22
% #define CTRL_REG4 0x23
% #define CTRL_REG5 0x24
% #define REGISTRE_MSB_X 0x29 // MSB axe X
% #define REGISTRE_LSB_X 0x28 // LSB axe X
% #define REGISTRE_MSB_Y 0x2B // MSB axe Y
% #define REGISTRE_LSB_Y 0x2A // LSB axe Y
% #define REGISTRE_MSB_Z 0x2D // MSB axe Z
% #define REGISTRE_LSB_Z 0x2C // LSB axe Z


% CTRL_REG4 (23h)
% Table 30.
% BDU
% BLE 
% FS1 ----------> 10: 2000 dps; 11: 2000 dps  2000dps in both cases
% FS0 ----------> 10: 2000 dps; 11: 2000 dps 2000dps in both cases
 % -
% ST1
% ST0
% SIM
%   CTRL_REG4 description
% BDU
% Block Data Update. Default value: 0
% (0: continous update; 1: output registers not updated until MSB and LSB reading)
% BLE
% Big/Little Endian Data Selection. Default value 0.
% (0: Data LSB @ lower address; 1: Data MSB @ lower address)
% FS1-FS0
% Full Scale selection. Default value: 00
% (00: 250 dps; 01: 500 dps; 10: 2000 dps; 11: 2000 dps)
% ST1-ST0
% Self Test Enable. Default value: 00
% (00: Self Test Disabled; Other: See Table )
% SIM
% SPI Serial Interface Mode selection. Default value: 0 (0: 4-wire interface; 1: 3-wire interface).
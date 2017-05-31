#!/usr/bin/perl

$PROG_NAME = shift || die "add param!\n";
@PROG_PARAMS = @ARGV;

$ERR_FILE="./tmp";
$OUT_FILE="./out";
$DEL_SEM_FILE="./delsem.pl";

$was_addr = 1;

while( $was_addr == 1) 
{
    system("perl ${DEL_SEM_FILE}");
    system("./make_1.sh ${PROG_NAME}");
    system("rm $ERR_FILE $OUT_FILE");
    system("echo ./${PROG_NAME}new 2>${ERR_FILE} 1>./out");
    system("     ./${PROG_NAME}new 2>${ERR_FILE} 1>./out");

    $was_addr = 0;
    open f, "./tmp" or die "Error open file!\n";
    PROCESS: while(<f>) {
        if( /(0x[0-9a-f]+)/ ) {
            $addr = $1;
            print "=======> Found addr - ".$addr."\n";
            open xml, "+< ./${PROG_NAME}.xml" or die "Xml not found\n";
            $out = "";
            while(<xml>) {
                if( /<\!-- eauto -->/ ) {
                    $out .= "<entry addr=\"$addr\"/>\n";
                }
                $out .= $_;
            }
            seek xml,0,0;
            print xml $out;
            truncate xml, tell(xml);
            close xml;
            $was_addr = 1;
            last PROCESS;
        }
    }
    close $f;
}

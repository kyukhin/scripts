#!/usr/bin/perl

$test_name;
$args_num = $#ARGV;
$xml_file;
$xml_file_new;
$line;

if ( $args_num < 0)
{
	print( "Usage: patch_xml <test_name>\n");
	print( "   or: patch_xml <xml_file> <dump_file>\n");
	exit( 1);
}

if ( $args_num == 0 )
{
	$test_name = $ARGV[0];

	$xml_file = "$test_name.xml";
	$dump_file = "$test_name.dump";
}
else
{
	$xml_file = $ARGV[0];
	$dump_file = $ARGV[1];
}

$xml_file_new = "$xml_file.patched.xml";

print("Input xml file: $xml_file\n");
print("Input dump file: $dump_file\n");
print("Output xml file: $xml_file_new\n");

open( FH_XML, "< $xml_file") or die "Cann't open $xml_file\n";
open( FH_DUMP, "< $dump_file") or die "Cann't open $dump_file\n";
open( FH_XML_NEW, "> $xml_file_new") or die "Cann't create $xml_file_new\n";

print("\nProcessing started...\n");

print("  Reading dump... ");

%hash;

foreach (<FH_DUMP>)
{
    if( (!/[0-9a-fA-F]{2}\s[0-9a-fA-F]{2}\s+$/) && (! /[0-9a-fA-F]+:\t[0-9a-fA-F]{2}\s+$/) )
    {
	if ( m/([0-9a-fA-F]+):.*/ )
	{
		$hash{$1} = "1";
                # jMP addr recognizing
                if( m/j[a-z]{2}\s+([0-9a-fA-F]{7})/ )	
                {
                   $hash{$1} = "1";
                    #print $_;                    
                }
        }
    }
}

close( FH_DUMP);
print("done\n");


print("  Reading xml... ");

my @content = <FH_XML>;

close( FH_XML);
print("done\n");


my $transl = 0;
my $del = 0;
foreach $line (@content)
{
	$line =~ s/<region.*//;
	$line =~ s/<\/region.*//;
	$line =~ s/<\/node.*//;

	if ( $line =~ m/\s*<node addr=\"0x([0-9a-fA-F]+)\".*/ )
	{
		if ( exists $hash{$1} )
		{
			$line = "<entry addr=\"0x$1\"/>";
			++$transl;
		}
		else
		{
#			print( "  Node by addr=0x$1 was deleted\n");                        
			$line = "";
			++$del;
		}
	}

	if ( !( $line eq "") )
	{
		print FH_XML_NEW $line;
	}
}

close( FH_XML_NEW);

print("Total: $transl nodes translated\n");
print("       $del nodes deleted\n");
print("Processing finished.\n");

use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
              AUTHOR            => 'Theo Lengyel <dirt@cpan.org>',
              NAME		=> 'Tie::Strand',
              VERSION_FROM	=> 'Strand.pm',
              ABSTRACT_FROM     => 'Strand.pm',
              PREREQ_PM		=> { 'Log::Easy'   => '.01',
                                     'Tie::Silk'   => '.01',
                                   },
              PM                => { 'Strand.pm' => '$(INST_LIBDIR)/Strand.pm'},
             );

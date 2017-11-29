/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
#ifndef ir_elf_INCLUDED
#define ir_elf_INCLUDED

// typedefs used to instantiate templates that are based on the format of
// an Elf object

struct ELF64
{
    typedef Elf64_Off	Elf_Off;
    typedef Elf64_Ehdr	Elf_Ehdr;
    typedef Elf64_Shdr	Elf_Shdr;
    typedef Elf64_Sym	Elf_Sym;

    unsigned char Elf_class () const { return ELFCLASS64; }
    unsigned char Elf_st_info (UINT bind, UINT type) const {
	return ELF64_ST_INFO (bind, type);
    }
};


struct ELF32
{
    typedef Elf32_Off	Elf_Off;
    typedef Elf32_Ehdr	Elf_Ehdr;
    typedef Elf32_Shdr	Elf_Shdr;
    typedef Elf32_Sym	Elf_Sym;

    unsigned char Elf_class () const { return ELFCLASS32; }
    unsigned char Elf_st_info (UINT bind, UINT type) const {
	return ELF32_ST_INFO (bind, type);
    }
};

#if defined(__sgi) && defined(mips)
#define __ALWAYS_USE_64BIT_ELF__
#endif

#endif // ir_elf_INCLUDED

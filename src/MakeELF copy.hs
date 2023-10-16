

import Data.Binary

newtype ElfWord = ElfWord Word32 deriving (Show, Eq)
newtype ElfAddr = ElfAddr Word64 deriving (Show, Eq)
newtype ElfOff = ElfOff Word64 deriving (Show, Eq)
newtype ElfHalf = ElfHalf Word16 deriving (Show, Eq)
newtype ElfSword = ElfSword Int32 deriving (Show, Eq)
newtype ElfSxword = ElfSxword Int64 deriving (Show, Eq)
newtype ElfXword = ElfXword Word64 deriving (Show, Eq)
newtype ElfByte = ElfByte Word8 deriving (Show, Eq)

data ElfMachine = EM_NONE | EM_M32 | EM_SPARC | EM_386 | EM_68K | EM_88K | EM_860 
| EM_MIPS | EM_PARISC | EM_SPARC32PLUS | EM_PPC | EM_PPC64 | EM_S390 | EM_ARM 
| EM_SH | EM_SPARCV9 | EM_IA_64 | EM_X86_64 | EM_VAX deriving (Show, Eq)

data ElfVersion = EV_NONE | EV_CURRENT deriving (Show, Eq)

data ElfType = ET_NONE | ET_REL | ET_EXEC | ET_DYN | ET_CORE deriving (Show, Eq)

data ElfIdent = ElfIdent {
    ei_mag0 :: ElfByte, -- 0x7f   The first byte of the magic number
    ei_mag1 :: ElfByte, -- 'E'      The second byte of the magic number
    ei_mag2 :: ElfByte, -- 'L'      The third byte of the magic number
    ei_mag3 :: ElfByte, -- 'F'      The fourth byte of the magic number
    ei_class :: ElfByte, -- 1 for 32-bit, 2 for 64-bit  The fifth byte identifies the architecture for this binary
    ei_data :: ElfByte, -- 1 for little endian, 2 for big endian data encoding of the processor-specific data in the file.
    ei_version :: ElfByte, -- 1 for original ELF format
    ei_osabi :: ElfByte, -- 0 for System V
    ei_abiversion :: ElfByte, -- 0 for System V
    ei_pad :: [ElfByte] -- 7 bytes of padding
    } deriving (Show)

makeElfIndentX64 :: ElfIdent
makeElfIndentX64 = ElfIdent {
    ei_mag0 = 0x7f,
    ei_mag1 = 0x45,
    ei_mag2 = 0x4c,
    ei_mag3 = 0x46,
    ei_class = 0x02,
    ei_data = 0x01,
    ei_version = 0x01,
    ei_osabi = 0x00,
    ei_abiversion = 0x00,
    ei_pad = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
    }


-------------------------------------------------------------------------------
-- region ELF Section header
-------------------------------------------------------------------------------

data ElfSectionHeader = {
    sh_name :: ElfWord,
    sh_type :: ElfWord,
    sh_flags :: ElfXword,
    sh_addr :: ElfAddr,
    sh_offset :: ElfOff,
    sh_size :: ElfXword,
    sh_link :: ElfWord,
    sh_info :: ElfWord,
    sh_addralign :: ElfXword,
    sh_entsize :: ElfXword
    } deriving (Show)

-- les couleur sont KC

makeDefaultElfSection :: ElfSectionHeader
makeDefaultElfSection = ElfSectionHeader {
    sh_name = 0, -- name of the section
    sh_type = 0, -- type of the section
    sh_flags = 0, -- flags
    sh_addr = 0, -- virtual address
    sh_offset = 0, -- offset in file
    sh_size = 0, -- size in file
    sh_link = 0, -- section header table index link
    sh_info = 0, -- extra information
    sh_addralign = 0, -- address alignment
    sh_entsize = 0 -- size of entries, if section has table
    }

-------------------------------------------------------------------------------
-- region ELF program header
-------------------------------------------------------------------------------

data ElfProgramHeader = {
    p_type :: ElfWord,
    p_offset :: ElfOff,
    p_flags :: ElfWord,
    p_vaddr :: ElfAddr,
    p_paddr :: ElfAddr,
    p_filesz :: ElfXword,
    p_memsz :: ElfXword,
    p_align :: ElfXword
    } deriving (Show)

makeDefaultElfProgram :: ElfProgramHeader
makeDefaultElfProgram = ElfProgram {
    p_type = 0, -- PT_NULL
    p_offset = 0, -- offset in file
    p_flags = 0, -- flags
    p_vaddr = 0, -- virtual address
    p_paddr = 0, -- physical address
    p_filesz = 0, -- size in file
    p_memsz = 0, -- size in memory
    p_align = 0 -- alignment
    }

---------------------------------------------------------
-- region ELF header
---------------------------------------------------------

data ElfHeader = ElfHeader {
    e_indent :: ElfIdent,
    e_type :: ElfType,
    e_machine :: ElfMachine, -- architecture of the machine, e.g. x86_64
    e_version :: ElfVersion,
    e_entry :: ElfAddr,
    e_phoff :: ElfOff,
    e_shoff :: ElfOff,
    e_flags :: ElfWord,
    e_ehsize :: ElfHalf,
    e_phentsize :: ElfHalf,
    e_phnum :: ElfHalf,
    e_shentsize :: ElfHalf,
    e_shnum :: ElfHalf,
    e_shstrndx :: ElfHalf
    } deriving (Show)

-- makes a default elf header with a single parameter for the architecture
makeDefaultElfHeader :: ElfMachine -> ElfHeader
makeDefaultElfHeader machine =
  ElfHeader
    { e_ident = makeElfIndentX64,
      e_type = 2, -- ET_EXEC
      e_machine = machine,
      e_version = 1,
      e_entry = 0, -- entry point virtual address, to define later once it is known
      e_phoff = 0, -- program header table file offset, to define later once it is known
      e_shoff = 0, -- section header table file offset, to define later once it is known
      e_flags = 0, -- processor-specific flags
      e_ehsize = 52, -- ELF header size in bytes, compute later
      e_phentsize = 0, -- size of program header table entry
      e_phnum = 0, -- number of entries in program header table
      e_shentsize = 0, -- size of section header table entry
      e_shnum = 0, -- number of entries in section header table
      e_shstrndx = 0 -- section header table index of section name string table
    }
-------------------------------------------------------------------------------
-- region ELF file
-------------------------------------------------------------------------------

data ElfFile = ElfFile {
    elf_header :: ElfHeader,
    elf_sectionsHeaders :: [ElfSectionHeader],
    elf_programsHeaders :: [ElfProgramHeader],
    elf_sections :: [ElfByte]
    elf_programs :: [ElfByte]
} deriving (Show)

--  c pas comme ca le délire ?
-- jsp maintenant chat dit que ça va direct dans les sections XD
-- par contre ca veut dire qu'il nous manque un champ dans la section pour le binaire
-- pour moi tu as le ptr vers le program/section header
-- et dans le pr/sec header tu a le pointeur vers les datas

newElfFile :: ElfFile
newElfFile = ElfFile {
    elf_header = makeDefaultElfHeader EM_X86_64, 
    elf_sections = [],
    elf_programs = []
}

use std::{
    io::{self, Write},
    mem,
};

use byteorder::{LE, WriteBytesExt};

use crate::OBJECT_HEADER_MAGIC;

use super::{BytecodeBlob, CodeUnit, ResolvedRelocs, UnitSection};

/// Maximum size of the code unit filename field
const CODE_UNIT_NAME_SIZE: usize = 255;

pub(crate) fn encode_bytecode(out: &mut impl io::Write, blob: &BytecodeBlob) -> io::Result<()> {
    // Start with emitting header
    out.write_all(OBJECT_HEADER_MAGIC)?;

    // Emit TProlog preferences:
    // - close_window_on_terminate
    out.write_u32::<LE>(0)?;
    // - run_with_args
    out.write_u32::<LE>(0)?;
    // - center_window
    out.write_u32::<LE>(0)?;
    // - dont_terminate
    out.write_u32::<LE>(0)?;

    // Emit Environment preferences
    let env_prefs = write_env_prefs(blob)?;
    out.write_all(&env_prefs)?;

    // Emit trailer signature
    out.write_all(OBJECT_HEADER_MAGIC)?;

    for unit in blob.code_units() {
        write_code_unit(out, unit)?;
    }

    if let Some(main_unit) = blob.main_unit() {
        // Write main unit trailer
        const MAIN_UNIT_MAGIC: &[u8] = b"***MAIN PROGRAM***\0";

        out.write_u16::<LE>(main_unit.as_usize() as u16)?;
        out.write_all(MAIN_UNIT_MAGIC)?;
        out.write_all(&vec![0u8; CODE_UNIT_NAME_SIZE - MAIN_UNIT_MAGIC.len()])?;
    }

    Ok(())
}

fn write_code_unit(out: &mut impl Write, unit: &CodeUnit<ResolvedRelocs>) -> io::Result<()> {
    // - file_no
    out.write_u16::<LE>(unit.id().as_usize() as u16)?;
    // - file_name
    {
        // note: We assume that the name will be UTF-8 encoded, which would only be
        // true for tulip-vm.
        let mut name_bytes = io::Cursor::new(vec![0u8; CODE_UNIT_NAME_SIZE]);

        if let Some((last_pos, last_char)) = unit
            .file_name()
            .char_indices()
            .take_while(|(index, _)| *index < CODE_UNIT_NAME_SIZE - 1)
            .last()
        {
            let truncated_name = &unit.file_name()[0..(last_pos + last_char.len_utf8())];
            name_bytes.write_all(truncated_name.as_bytes())?;
        }

        out.write_all(&name_bytes.into_inner())?;
    }
    // - body_no
    out.write_u16::<LE>(unit.body_unit.map_or(0, |it| it.as_usize() as u16))?;
    // - stub_no
    out.write_u16::<LE>(unit.stub_unit.map_or(0, |it| it.as_usize() as u16))?;

    // - code_size
    // Take into account reserved space at the beginning
    let code_size = unit.procedures().map(|it| it.size()).sum::<usize>() + 4;
    let code_size: u32 = code_size
        .try_into()
        .map_err(|_| io::Error::other("code section size is too big"))?;
    out.write_u32::<LE>(code_size)?;

    // - code_blob
    // Write reserved patch pointer
    out.write_u32::<LE>(0xA5A5A5A5)?;
    // Write out the rest of the procedures
    for procedure in unit.procedures() {
        // Instructions first
        for instr in procedure.instrs() {
            instr.encode(out)?;
        }

        // Followed by case tables
        for case_table in procedure.case_tables() {
            out.write_u32::<LE>(case_table.lower_bound)?;
            out.write_u32::<LE>(case_table.upper_bound)?;
            out.write_u32::<LE>(case_table.default_branch)?;

            for arm in &case_table.arm_branches {
                out.write_u32::<LE>(*arm)?;
            }
        }
    }

    // - manifest_size
    let manifest_size: u32 = unit
        .manifest()
        .size()
        .try_into()
        .map_err(|_| io::Error::other("manifest section size is too big"))?;
    out.write_u32::<LE>(manifest_size)?;
    // - manifest_blob
    out.write_all(unit.manifest().data())?;

    // - globals_size
    let globals_size: u32 = unit
        .globals()
        .size()
        .try_into()
        .map_err(|_| io::Error::other("globals section size is too big"))?;
    out.write_u32::<LE>(globals_size)?;

    // - manifest_patches
    // TODO: write manifest patches
    out.write_u32::<LE>(0xFFFFFFFF)?; // manifest patch list tail

    // - local_patches
    let sections = &[
        UnitSection::Code,
        UnitSection::Manifest,
        UnitSection::Global,
    ];

    for section in sections {
        let patch_head = unit.local_relocs_start(*section).unwrap_or(0);
        out.write_u32::<LE>(patch_head)?;
    }

    // - external_patches
    for section in sections {
        if let Some(external_patches) = unit.external_relocs_start(*section) {
            for (code_unit, patch_head) in external_patches {
                out.write_u16::<LE>(code_unit.as_usize() as u16)?;
                out.write_u32::<LE>(*patch_head)?;
            }
        }
        out.write_u16::<LE>(0xFFFF)?;
    }

    Ok(())
}

fn write_env_prefs(_blob: &BytecodeBlob) -> io::Result<Vec<u8>> {
    let mut env_prefs = io::Cursor::new(vec![0u8; env_prefs::ENV_PREFS_SIZE]);
    // - use_graphics_mode: True
    env_prefs
        .set_position(mem::offset_of!(env_prefs::OpenTuringPrologue, use_graphics_mode) as u64);
    env_prefs.write_u32::<LE>(1)?;

    // - run_console_font_name: Courier New
    env_prefs
        .set_position(mem::offset_of!(env_prefs::OpenTuringPrologue, run_console_font_name) as u64);
    env_prefs.write_all(b"Courier New\0")?;

    // - run_console_font_size: 10
    env_prefs
        .set_position(mem::offset_of!(env_prefs::OpenTuringPrologue, run_console_font_size) as u64);
    env_prefs.write_u32::<LE>(10)?;

    // - run_console_full_screen: False
    env_prefs.set_position(
        mem::offset_of!(env_prefs::OpenTuringPrologue, run_console_full_screen) as u64,
    );
    env_prefs.write_u32::<LE>(0)?;

    // - run_console_text_rows: 25
    env_prefs
        .set_position(mem::offset_of!(env_prefs::OpenTuringPrologue, run_console_text_rows) as u64);
    env_prefs.write_u32::<LE>(25)?;

    // - run_console_text_cols: 80
    env_prefs
        .set_position(mem::offset_of!(env_prefs::OpenTuringPrologue, run_console_text_cols) as u64);
    env_prefs.write_u32::<LE>(80)?;

    // - run_use_small_font: True
    env_prefs
        .set_position(mem::offset_of!(env_prefs::OpenTuringPrologue, run_use_small_font) as u64);
    env_prefs.write_u32::<LE>(1)?;

    // - turing_stack_size_in_kb: 0 (default)
    env_prefs.set_position(
        mem::offset_of!(env_prefs::OpenTuringPrologue, turing_stack_size_in_kb) as u64,
    );
    env_prefs.write_u32::<LE>(0)?;

    // - execution_delay: 0ms
    env_prefs.set_position(mem::offset_of!(env_prefs::OpenTuringPrologue, execution_delay) as u64);
    env_prefs.write_u32::<LE>(0)?;

    // - prohibit_sys_exec: False
    env_prefs
        .set_position(mem::offset_of!(env_prefs::OpenTuringPrologue, prohibit_sys_exec) as u64);
    env_prefs.write_u32::<LE>(0)?;

    // - no_sound: False
    env_prefs.set_position(mem::offset_of!(env_prefs::OpenTuringPrologue, no_sound) as u64);
    env_prefs.write_u32::<LE>(0)?;

    Ok(env_prefs.into_inner())
}

mod env_prefs {
    // There are 3 choices of known preferences lengths:
    // - 0x17D4 (Prologue 1.0.0, for Turing 4.0.3 to 4.0.4c)
    // - 0xFE8 (Prologue 4.0.4d, for Turing 4.0.4d? to 4.0.5, and unofficially to 4.1.1a)
    // - 0xFF4 (Prologue 4.1.2 / OT 1.0.x, for Turing 4.1.2 and OpenTuring 1.0.x)
    //
    // As we only support targeting OT 1.0.x, we'll have the header size fixed to that prologue version
    pub(crate) const ENV_PREFS_SIZE: usize = 0xFF4;

    const _: () = {
        if std::mem::size_of::<OpenTuringPrologue>() != ENV_PREFS_SIZE {
            panic!("prologue size mismatch")
        }
    };

    type Bool = u32;
    type FilePath = [u8; 1024];
    type DriveList = [u8; 27];
    type PropString = [u8; 128];
    type LongPropString = [u8; 1024];

    // Primarily inteded for getting the offsets of a few properties:
    #[repr(C)]
    pub(crate) struct OpenTuringPrologue {
        _properties_initialized: Bool,
        _startup_directory: FilePath,
        _use_my_documents: Bool,
        _students_can_change_user_name: Bool,
        _student_can_enter_advanced_mode: Bool,
        _student_restricted_to_startup_dir: Bool,
        _student_drives_allowed: Bool,
        _student_drives_allowed_list: DriveList,
        _student_drives_forbidden: Bool,
        _student_drives_forbidden_list: DriveList,
        _student_cant_change_prefs: Bool,
        _beginner_mode: Bool,
        _one_window_mode: Bool,
        _use_full_path_names: Bool,
        _edit_save_backup: Bool,
        _edit_indent_size: u32,
        _confirm_quit: Bool,
        _skip_splash_screen: Bool,
        _add_suffix_to_files_with_none: Bool,
        _edit_leading_spaces_to_tabs: Bool,
        _edit_open_brace_on_same_line: Bool,
        _edit_num_spaces_per_tab: u32,
        _no_recent_files: Bool,
        _use_last_directory: Bool,
        _find_uses_selection: Bool,
        _display_debugger_menu: Bool,
        _display_window_menu: Bool,
        _edit_font_name: PropString,
        _edit_font_size: u32,
        _edit_syntax_colouring: Bool,
        _edit_beginner_full_screen: Bool,
        _edit_advanced_full_screen: Bool,
        _edit_text_rows: u32,
        _edit_text_cols: u32,
        _edit_caret_width: u32,
        _debugger_menu: Bool,
        _window_menu: Bool,
        pub(crate) use_graphics_mode: Bool,
        pub(crate) run_console_font_name: PropString,
        pub(crate) run_console_font_size: u32,
        pub(crate) run_console_full_screen: Bool,
        pub(crate) run_console_text_rows: u32,
        pub(crate) run_console_text_cols: u32,
        _run_applet_width: u32,
        _run_applet_height: u32,
        pub(crate) run_use_small_font: Bool,
        _additional_class_path: LongPropString,
        _jikes_options: LongPropString,
        _start_java_runner_in_debugger: Bool,
        _memory_for_jvm_in_m_b: u32,
        _old_java_compile: Bool,
        _jvm_type: u32,
        _version_in_registry: [u8; 20],
        /// Stack size per executor (at least 64 KiB)
        pub(crate) turing_stack_size_in_kb: u32,
        /// Startup Delay (in milliseconds)
        pub(crate) execution_delay: u32,
        // IO & Parallel ports are not supported
        _parallel_io_port: u32,
        /// If Sys.Exec should be disabled
        pub(crate) prohibit_sys_exec: Bool,
        /// If sound-related modules should be disabled
        pub(crate) no_sound: Bool,
        _pru32_font_name: PropString,
        _user_name: PropString,
        _pru32_font_size: u32,
        _pru32_bold_keywords: Bool,
        _pru32_italicize_idents: Bool,
        _pru32_page_header: Bool,
        _pru32_user_name: Bool,
        _pru32_line_numbers: Bool,
        _pru32_header_font_name: PropString,
        _pru32_header_font_size: u32,
        _pru32_border: Bool,
        _pru32_margin_left: u32,
        _pru32_margin_right: u32,
        _pru32_margu32op: u32,
        _pru32_margin_bottom: u32,
        _pru32_two_up: Bool,
        _num_recent_files: u32,
        _edit_one_window_margin: u32,
        _catch_exceptions: Bool,
        _debugger_available: Bool,
        _debug: Bool,
        _debug_window_areas: Bool,
        _logging: Bool,
        _log_window_messages: Bool,
        _stream0_to_stderr: Bool,
        _errors_to_stderr: Bool,
        _info_to_stderr: Bool,
    }
}

@echo off
setlocal

cd /d "%~dp0"

for %%a in (%*) do set "%%a=1"

if not "%release%" == "1" set debug=1
if "%debug%" == "1"       set release=0 && echo [debug build]
if "%release%" == "1"     set debug=0   && echo [release build]

set common_compiler_flags=/nologo /we4047
:: warnings as errors: /WX

set debug_flags=%common_compiler_flags% /Zi
set release_flags=%common_compiler_flags% /O2 /D RADDBG_MARKUP_STUBS

if "%debug%"   == "1" set compiler_flags=%debug_flags% && set sub_dir=debug
if "%release%" == "1" set compiler_flags=%release_flags% && set sub_dir=release

cl src/main.c /Fe:kk.exe %compiler_flags% || exit /b 1
echo [kk.exe]

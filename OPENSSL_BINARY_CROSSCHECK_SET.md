# OpenSSL Binary Translation Cross-Check Set

This file is a manual audit set for `BinRevX` binary-to-`MicroIR` translation output.

Selection policy:
- `Best complex`: functions where the importer/recovery pipeline currently shows the strongest structural recovery on nontrivial OpenSSL internals.
- `Worst simple`: source-simple wrapper/driver functions where the recovered result is still noisier than it should be.

Source file paths below come from DWARF / `addr2line` on the binaries in `bins/`.

## Best Complex

### 1. `fill_block`
- Binary: `bins/ecdsa_sign_openssl-O0_32`
- Size: `11027` bytes
- Source file: `openssl-3.5.0-32-O0/providers/implementations/kdfs/argon2.c:374`
- Why selected: two recovered loops, recovered branch split, nontrivial call summary

```text
Function fill_block:
  signature: params=[arg0,arg1,arg2,arg3] ret=eax
  loop(header=L_12437d, guard=(local_828 le 7), shape=two-block, body=2, exits=1, carried=7)
  loop(header=L_122d7c, guard=(local_828 le 7), shape=two-block, body=2, exits=1, carried=7)
    if(header=L_1218c1, guardT=(arg3 eq 0), guardF=(arg3 ne 0), then=L_12195a, else=L_121942)
    if(header=L_12438a, guardT=(flag eq 0), guardF=(flag ne 0), then=L_1243cc, else=L_1243c7)
  summary(fill_block): params=[arg0,arg1,arg2,arg3] pre=[copy_block,eax_eax_1,local_41c,local_81c,local_c,mul_lower,rotr64,xor_block] post=[loops=2,ret=eax] modifies=[eax,local_1c,local_820,local_824,local_828,memory] calls=[copy_block,mul_lower,rotr64,xor_block]
```

### 2. `sha1_block_data_order`
- Binary: `bins/ecdsa_sign_openssl-O0_32`
- Size: `6746` bytes
- Source file: `openssl-3.5.0-32-O0/crypto/sha/sha_local.h:138`
- Why selected: recovered loop, recovered induction roles, invariants and VC generation present

```text
Function sha1_block_data_order:
  signature: params=[arg0,arg1,arg2] ret=eax
  loop(header=L_246ebd, guard=(local_14 ne 0), shape=natural, body=6, exits=1, carried=30)
    if(header=L_246ece, guardT=(eax ne eax), guardF=(eax eq eax), then=L_2471ee, else=L_246edc)
    if(header=L_2479ae, guardT=(arg2 eq 0), guardF=(arg2 ne 0), then=L_2488d8, else=L_2488ad)
  summary(sha1_block_data_order): params=[arg0,arg1,arg2] pre=[ebx_eax_1,ecx_eax_1,edi_eax_1,edx_eax_1,esi_eax_1] post=[loops=1,ret=eax] modifies=[eax,local_10,local_14,local_18,local_1c,local_20,local_24,local_28,local_2c,local_30,local_34,local_38,local_3c,local_40,local_44,local_48,local_4c,local_50,local_54,local_58,local_60,local_64,local_68,local_6c,memory] calls=[]
    loop-summary(... roles=[induction=arg2,local_14])
    invariants(header=L_246ebd): [(local_14 != 0); arg2 is monotone decreasing; arg2 is the induction variable; local_14 is monotone increasing; local_14 is the induction variable; ...]
    vcs(scope=L_246ebd): init ... | preservation ... | exit ...
```

### 3. `BN_mod_sqrt`
- Binary: `bins/ecdsa_sign_openssl-O0_32`
- Size: `2584` bytes
- Source file: `openssl-3.5.0-32-O0/crypto/bn/bn_sqrt.c:20`
- Why selected: many loop candidates and deep conditional recovery on a compact cryptographic routine

```text
Function BN_mod_sqrt:
  signature: params=[arg0,arg1,arg2,arg3] ret=eax
  loop(header=L_19174a, guard=(eax eq eax), shape=natural, body=20, exits=9, carried=4)
  loop(header=L_191874, guard=(local_2c gt 0), shape=natural, body=3, exits=2, carried=1)
  loop(header=L_1917e5, guard=(eax lt local_34), shape=natural, body=6, exits=4, carried=2)
  loop(header=L_19141d, guard=(local_30 gt 21), shape=natural, body=13, exits=8, carried=3)
  loop(header=L_191241, guard=(eax eq eax), shape=two-block, body=2, exits=1, carried=2)
    if(header=L_191004, guardT=(eax eq eax), guardF=(eax ne eax), then=L_191048, else=L_191030)
    if(header=L_19105c, guardT=(arg0 ne 0), guardF=(arg0 eq 0), then=L_19106a, else=L_191062)
    if(header=L_19117b, guardT=(local_10 eq 0), guardF=(local_10 ne 0), then=L_191976, else=L_191200)
```

### 4. `rsa_multiprime_keygen`
- Binary: `bins/ecdsa_sign_openssl-O0_32`
- Size: `3372` bytes
- Source file: `openssl-3.5.0-32-O0/crypto/rsa/rsa_gen.c:265`
- Why selected: strongest high-complexity structural recovery currently observed, with many loops and branch regions recovered

```text
Function rsa_multiprime_keygen:
  signature: params=[arg0,arg1,arg2,arg3,arg4] ret=eax
  loop(header=L_c020f, guard=(eax lt arg2), shape=natural, body=4, exits=3, carried=4)
  loop(header=L_c003d, guard=(eax lt arg2), shape=natural, body=4, exits=3, carried=5)
  loop(header=L_bfee8, guard=(eax lt arg2), shape=natural, body=52, exits=19, carried=15)
  loop(header=L_bfb3a, guard=(eax eq eax), shape=natural, body=26, exits=11, carried=9)
  loop(header=L_bfbd4, guard=(eax lt local_74), shape=natural, body=8, exits=2, carried=3)
  loop(header=L_bfa9a, guard=(eax lt arg2), shape=natural, body=3, exits=2, carried=3)
  loop(header=L_bf8c1, guard=(eax lt arg2), shape=natural, body=5, exits=1, carried=1)
    if(header=L_bf5fb, guardT=(arg1 gt 511), guardF=(arg1 le 511), then=L_bf6f2, else=L_bf6b8)
    if(header=L_bf787, guardT=(arg2 le 1), guardF=(arg2 gt 1), then=L_bf7a0, else=L_bf78d)
    if(header=L_bf7dd, guardT=(local_60 ne 0), guardF=(local_60 eq 0), then=L_bf7f5, else=L_bf7eb)
```

### 5. `build_chain`
- Binary: `bins/ecdsa_sign_openssl-O0_32`
- Size: `2658` bytes
- Source file: `openssl-3.5.0-32-O0/crypto/x509/x509_vfy.c:3282`
- Why selected: one recovered loop plus a large number of recovered conditional regions

```text
Function build_chain:
  signature: params=[arg0] ret=eax
  loop(header=L_d894d, guard=(eax gt local_20), shape=two-block, body=2, exits=1, carried=2)
    if(header=L_d8580, guardT=(local_40 ne 1), guardF=(local_40 eq 1), then=L_d8604, else=L_d85f2)
    if(header=L_d8619, guardT=(local_18 eq 0), guardF=(local_18 ne 0), then=L_d863d, else=L_d8630)
    if(header=L_d8650, guardT=(local_38 eq 0), guardF=(local_38 ne 0), then=L_d8668, else=L_d8656)
    if(header=L_d868e, guardT=(local_3c ne 0), guardF=(local_3c eq 0), then=L_d86d4, else=L_d869c)
    if(header=L_d87a0, guardT=(eax le 1073741823), guardF=(eax gt 1073741823), then=L_d87bd, else=L_d87b0)
```

## Worst Simple

### 1. `tester_main`
- Binary: `bins/ecdsa_keygen_openssl-O0_32`
- Size: `393` bytes
- Source file: `/home/faisal/code/lsl/sct/benchmark/32/openssl-O0/ecdsa_keygen/src/wrapper.c:92`
- Why selected: source-simple wrapper logic, but recovered summary still contains a lot of API/global string noise

```text
Function tester_main:
  signature: params=[arg0] ret=eax
  no loop recovered
    if(header=L_69d4e, guardT=(local_14 ne 0), guardF=(local_14 eq 0), then=L_69dce, else=L_69d94)
    if(header=L_69dce, guardT=(eax ne eax), guardF=(eax eq eax), then=L_69e28, else=L_69de0)
    if(header=L_69e28, guardT=(eax ne eax), guardF=(eax eq eax), then=L_69e9a, else=L_69e5b)
  summary(tester_main): params=[arg0] pre=[bio_free,bio_new,bio_read,bio_s_mem,ec_key_free,ec_key_generate_key,ec_key_new_by_curve_name,err_print_errors_fp,fwrite@plt,global_rand_idx,got_stderr,local_4,pem_write_bio_ecprivatekey,record_mode,str_EC_KEY_generate_key_failed_in_te,str_EC_KEY_new_by_curve_name_failed,str_PEM_write_error] post=[ret=eax] modifies=[eax,local_10,local_14,local_c,memory] calls=[bio_free,bio_new,bio_read,bio_s_mem,ec_key_free,ec_key_generate_key,ec_key_new_by_curve_name,err_print_errors_fp,fwrite@plt,pem_write_bio_ecprivatekey]
```

### 2. `tester_main`
- Binary: `bins/eddsa_keygen_openssl-O0_32`
- Size: `561` bytes
- Source file: `/home/faisal/code/lsl/sct/benchmark/32/openssl-O0/eddsa_keygen/src/wrapper.c:98`
- Why selected: simple wrapper/test harness, but recovery is still dominated by library-call noise

```text
Function tester_main:
  signature: params=[arg0] ret=eax
  no loop recovered
    if(header=L_69ddb, guardT=(local_18 ne 0), guardF=(local_18 eq 0), then=L_69e75, else=L_69e3b)
    if(header=L_69e75, guardT=(eax gt eax), guardF=(eax le eax), then=L_69ecf, else=L_69e87)
    if(header=L_69ecf, guardT=(eax gt eax), guardF=(eax le eax), then=L_69f2d, else=L_69ee5)
    if(header=L_69f2d, guardT=(eax ne eax), guardF=(eax eq eax), then=L_69faf, else=L_69f61)
  summary(tester_main): params=[arg0] pre=[bio_free,bio_new,bio_read,bio_s_mem,err_print_errors_fp,evp_pkey_ctx_free,evp_pkey_ctx_new_id,evp_pkey_free,evp_pkey_keygen,evp_pkey_keygen_init,fwrite@plt,global_rand_idx,got_stderr,local_4,pem_write_bio_privatekey,record_mode,str_EVP_PKEY_CTX_new_id_failed,str_EVP_PKEY_keygen_failed_in_tester,str_EVP_PKEY_keygen_init_failed,str_PEM_write_error] post=[ret=eax] modifies=[eax,local_10,local_14,local_18,local_1c,local_c,memory] calls=[bio_free,bio_new,bio_read,bio_s_mem,err_print_errors_fp,evp_pkey_ctx_free,evp_pkey_ctx_new_id,evp_pkey_free,evp_pkey_keygen,evp_pkey_keygen_init,fwrite@plt,pem_write_bio_privatekey]
```

### 3. `tester_main`
- Binary: `bins/rsa_keygen_openssl-O0_32`
- Size: `352` bytes
- Source file: `/home/faisal/code/lsl/sct/benchmark/32/openssl-O0/rsa_keygen/src/wrapper.c:92`
- Why selected: source-simple wrapper, but imported state still exposes many OpenSSL runtime details instead of a small wrapper contract

```text
Function tester_main:
  signature: params=[arg0] ret=eax
  no loop recovered
    if(header=L_69d33, guardT=(eax ne eax), guardF=(eax eq eax), then=L_69df9, else=L_69da3)
    if(header=L_69df9, guardT=(eax ne eax), guardF=(eax eq eax), then=L_69e48, else=L_69e2c)
  summary(tester_main): params=[arg0] pre=[bio_free,bio_new,bio_read,bio_s_mem,bn_free,bn_new,bn_set_word,err_print_errors_fp,fwrite@plt,global_rand_idx,got_stderr,local_4,pem_write_bio_rsaprivatekey,record_mode,rsa_free,rsa_generate_key_ex,rsa_new,str_PEM_write_error,str_RSA_generate_key_ex_failed_in_te] post=[ret=eax] modifies=[eax,local_10,local_14,local_18,local_c,memory] calls=[bio_free,bio_new,bio_read,bio_s_mem,bn_free,bn_new,bn_set_word,err_print_errors_fp,fwrite@plt,pem_write_bio_rsaprivatekey,rsa_free,rsa_generate_key_ex,rsa_new]
```

### 4. `main`
- Binary: `bins/ecdsa_sign_openssl-O0_32`
- Size: `270` bytes
- Source file: `/home/faisal/code/lsl/sct/benchmark/32/openssl-O0/ecdsa_sign/src/main_template.c:10`
- Why selected: source-simple driver loop, but recovery still frames the loop through call-shaped bounds and low-level temporaries

```text
Function main:
  signature: params=[arg0,arg1,arg2] ret=eax
  loop(header=L_6a388, guard=(eax lt local_210), shape=two-block, body=2, exits=1, carried=3)
    if(header=L_6a2b5, guardT=(local_210 gt 0), guardF=(local_210 le 0), then=L_6a336, else=L_6a313)
    if(header=L_6a3a8, guardT=(flag eq 0), guardF=(flag ne 0), then=L_6a3b9, else=L_6a3b4)
  summary(main): params=[arg0,arg1,arg2] pre=[PLAINTEXT,PLAINTEXT_LEN,ecdsa_sign_tester,fwrite@plt,got_stderr,local_20c,local_8,printf@plt,putchar@plt,str_0x_02x_,str_ECDSA_signing_failed,str_Signature___d_bytes__] post=[loops=1,ret=eax] modifies=[eax,local_210,local_214,local_c] calls=[ecdsa_sign_tester,fwrite@plt,printf@plt,putchar@plt]
    loop-summary(... roles=[induction=local_214; folded=eax->local_214])
    invariants(header=L_6a388): [(local_214 < sym_ecdsa_sign_tester(addr(sym_PLAINTEXT,0))); ...]
```

### 5. `main`
- Binary: `bins/rsa_sign_openssl-O0_32`
- Size: `246` bytes
- Source file: `/home/faisal/code/lsl/sct/benchmark/32/openssl-O0/rsa_sign/src/main_template.c:36`
- Why selected: same source shape as a small output loop, but current recovery still anchors the loop bound to the tester call rather than a cleaner byte-count abstraction

```text
Function main:
  signature: params=[arg0,arg1,arg2] ret=eax
  loop(header=L_6a34e, guard=(eax lt local_210), shape=two-block, body=2, exits=1, carried=3)
    if(header=L_6a293, guardT=(local_210 gt 0), guardF=(local_210 le 0), then=L_6a314, else=L_6a2f1)
    if(header=L_6a36e, guardT=(flag eq 0), guardF=(flag ne 0), then=L_6a37f, else=L_6a37a)
  summary(main): params=[arg0,arg1,arg2] pre=[PLAINTEXT,PLAINTEXT_LEN,fwrite@plt,got_stderr,local_20c,local_8,printf@plt,putchar@plt,rsa_sign_tester,str_0x_02x_,str_Operation_failed] post=[loops=1,ret=eax] modifies=[eax,local_210,local_214,local_c] calls=[fwrite@plt,printf@plt,putchar@plt,rsa_sign_tester]
    loop-summary(... roles=[induction=local_214; folded=eax->local_214])
    invariants(header=L_6a34e): [(local_214 < sym_rsa_sign_tester(addr(sym_PLAINTEXT,0))); ...]
```

## Notes

- The `best complex` group is still not semantically clean C. It is the current strongest structural recovery from binary among the sampled OpenSSL internals.
- The `worst simple` group is not failing to import; it is failing the stronger readability/modularity test. These are the places where source-simple logic still expands into noisy summaries.

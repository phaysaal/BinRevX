; ModuleID = '/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/string_copy_loop.c'
source_filename = "/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/string_copy_loop.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: nofree norecurse nosync nounwind memory(readwrite, inaccessiblemem: none) uwtable
define dso_local void @string_copy_loop(ptr nocapture noundef writeonly %0, ptr nocapture noundef readonly %1) local_unnamed_addr #0 {
  %3 = load i8, ptr %1, align 1, !tbaa !5
  %4 = icmp eq i8 %3, 0
  br i1 %4, label %13, label %5

5:                                                ; preds = %2, %5
  %6 = phi i8 [ %11, %5 ], [ %3, %2 ]
  %7 = phi ptr [ %10, %5 ], [ %1, %2 ]
  %8 = phi ptr [ %9, %5 ], [ %0, %2 ]
  store i8 %6, ptr %8, align 1, !tbaa !5
  %9 = getelementptr inbounds i8, ptr %8, i64 1
  %10 = getelementptr inbounds i8, ptr %7, i64 1
  %11 = load i8, ptr %10, align 1, !tbaa !5
  %12 = icmp eq i8 %11, 0
  br i1 %12, label %13, label %5, !llvm.loop !8

13:                                               ; preds = %5, %2
  %14 = phi ptr [ %0, %2 ], [ %9, %5 ]
  store i8 0, ptr %14, align 1, !tbaa !5
  ret void
}

attributes #0 = { nofree norecurse nosync nounwind memory(readwrite, inaccessiblemem: none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.module.flags = !{!0, !1, !2, !3}
!llvm.ident = !{!4}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{!"Ubuntu clang version 18.1.3 (1ubuntu1)"}
!5 = !{!6, !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C/C++ TBAA"}
!8 = distinct !{!8, !9, !10}
!9 = !{!"llvm.loop.mustprogress"}
!10 = !{!"llvm.loop.unroll.disable"}

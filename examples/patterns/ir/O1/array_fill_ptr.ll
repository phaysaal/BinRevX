; ModuleID = '/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/array_fill_ptr.c'
source_filename = "/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/array_fill_ptr.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: nofree norecurse nosync nounwind memory(write, inaccessiblemem: none) uwtable
define dso_local void @array_fill_ptr(ptr noundef writeonly %0, i32 noundef %1, i32 noundef %2) local_unnamed_addr #0 {
  %4 = sext i32 %1 to i64
  %5 = getelementptr inbounds i32, ptr %0, i64 %4
  %6 = icmp sgt i32 %1, 0
  br i1 %6, label %7, label %11

7:                                                ; preds = %3, %7
  %8 = phi ptr [ %9, %7 ], [ %0, %3 ]
  store i32 %2, ptr %8, align 4, !tbaa !5
  %9 = getelementptr inbounds i32, ptr %8, i64 1
  %10 = icmp ult ptr %9, %5
  br i1 %10, label %7, label %11, !llvm.loop !9

11:                                               ; preds = %7, %3
  ret void
}

attributes #0 = { nofree norecurse nosync nounwind memory(write, inaccessiblemem: none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.module.flags = !{!0, !1, !2, !3}
!llvm.ident = !{!4}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{!"Ubuntu clang version 18.1.3 (1ubuntu1)"}
!5 = !{!6, !6, i64 0}
!6 = !{!"int", !7, i64 0}
!7 = !{!"omnipotent char", !8, i64 0}
!8 = !{!"Simple C/C++ TBAA"}
!9 = distinct !{!9, !10, !11}
!10 = !{!"llvm.loop.mustprogress"}
!11 = !{!"llvm.loop.unroll.disable"}

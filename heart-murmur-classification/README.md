# Heart Murmur Classification

## Overview
Cardiac murmurs vary in intensity - soft (Levine grades I–III) versus loud (grades IV–VI) - and correctly grading them is clinically critical for referral decisions, yet existing automated systems either focus only on detecting whether a murmur is present or, when they do attempt grading, conflate detection and grading in a three-class framework that struggles badly with class imbalance and misclassifies soft murmurs at a high rate. To address this, we reframed grading as a binary classification task on murmur-present patients only, and built a CNN-Stacked BiLSTM with dilated convolutions, skip connections, and hybrid attention pooling, trained on MFCC features extracted from phonocardiogram recordings in the CirCor DigiScope dataset, systematically comparing it against five alternative architectures before tuning the final model. The result is a recording-level macro F1 of 0.903 - a 14.8-point improvement over the state-of-the-art baseline - with the most clinically meaningful gain on soft murmur classification (+27.2 F1 points), the class most dangerous to miss, suggesting this approach could meaningfully support low-resource cardiology screening where specialist access is scarce.

## Tech Stack
Python, PyTorch, librosa, soundfile, scikit-learn, NumPy, pandas, matplotlib, seaborn, Google Colab (CUDA/GPU)

## Key Results
- Achieved 92% recording-level accuracy and macro F1 of 0.903
- Outperformed Elola et al. (2023) baseline by 14.8 F1 points
- Soft murmur F1 of 0.940 vs. baseline 0.668 (+27.2 points)
- Loud murmur F1 of 0.860 vs. baseline 0.842
- CNN pre-processing was the single most impactful design choice (+0.0655 F1 over plain BiLSTM)
- Results stable across 1,000 bootstrap resamples (95% CI: [0.837, 0.962] recording-level)

## Files
- `heart-murmur-classification.ipynb`

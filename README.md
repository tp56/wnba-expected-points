# wnba-expected-points

## Project Structure
```
├── data/                   # Raw and processed data
├── src/                    # Source code
│   ├── calibration/        # Model calibration testing
│   ├── data/               # Data pipeline scripts
│   ├── model/              # Statistical modeling and analysis
│   └── visuals/            # Denerated visuals for paper
└── docs/                   # Documentation
```
## Getting Started
1. Pull data from WeHoop using src/data/pull.R
2. Run model/xP.R to build expected points model
3. Refomat data on team, player, shot level using residuals.R

## Data Sources
- All data was pulled from WeHoop and saved/used as an .rds and .csv using the script pull.R

## Reproducibility
- Data collection is automated using: pull.R
- Code to reproduce analysis is included in repository and modifiable for past and future seasons 

## Citation
If you use this in your work, please cite:
```bibtex
@inproceedings{wnba_expected_points,
  title={WxP: A Decomposed Expected Points Framework for WNBA Offense using Shots, Rebounds, and Fouls},
  author={Theresa Pham}
}
```

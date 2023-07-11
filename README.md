# OT Win Project: NBA 2002-2021

## Data Processing

- **Raw Data** were hand-crawled by four RAs from <https://www.basketball-reference.com>
- They collected data regarding all NBA games with OTs between 2002/03 season and 2021/22 season (20 years)

- Two RAs collected data at the game level:
  - Katherine Zhang: 2002-2011
  - Linda Zou: 2012-2021

- Two RAs collected data at the season level:
  - Kitty Nie: 2002-2011
  - Grace Cheang: 2012-2021

- As a result, the raw data contain idiosyncratic errors and systematic checks on the raw data were performed to correct the errors and generate **Initial Data**

- We then combine Game data and Season data. The data used for analysis takes the perspective of the Host team in a game  (**Data_HP**). We also wrote code that generates data that takes the perspective of the Forced team in a game (the team being forced into the 1st OT) for potential analysis

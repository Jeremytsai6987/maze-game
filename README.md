# Functional MLB Game: Your Journey to Glory

## Author Information

**Name:** Ya-Wei (Jeremy) Tsai  
**Date:** 12-01-2024

---

## Acknowledgments

Big thanks to:

- The University of Chicago Functional Programming course materials for their incredible support.

---

## Welcome to the Functional MLB Game!

You’re the General Manager (GM) of the **University of Chicago Team**, tasked with assembling the ultimate roster to claim the **2024 MLB World Series Championship**. Traverse iconic MLB stadiums, uncover hidden secrets, and prove your skills as a master strategist!

### Game Objectives

Win by achieving one of the following:

1. Assemble **three players from the same team** while staying under the salary cap.
2. **Encounter Shohei Ohtani** and defeat the legendary Babe Ruth in a thrilling showdown by correctly entering his career years (e.g., "????-????").

---

## Key Features

### Exciting New Commands

1. **`salary`:** Check the total salary of your roster.
2. **`team`:** See how many players from each team are on your roster.

### Locked Stadiums

- Some stadiums are locked!
  - Use a **Fenway Ticket** to enter **Fenway Park**.
  - Use a **Wrigley Key** to unlock **Wrigley Field**.
- Without these, you’ll get a message like:  
  _"The door is locked. You need a Fenway Ticket."_

### Hidden Challenge: Babe Ruth Showdown

- Encountering Shohei Ohtani activates a surprise challenge.
  - Correctly input Babe Ruth's career years to **win instantly**.
  - Incorrect guesses result in an immediate **game over**.

### Seamless Movement

- Navigate with simple commands like `north`, `south`, `east`, or `west`.
- Chain multiple commands using `and`:
  - Example: `north and east and take FenwayTicket`.

---

## How to Play and Win

### Map Exploration

- Traverse iconic MLB stadiums, including:
  - **Dodger Stadium**, **Fenway Park**, **Wrigley Field**, and more!
- Use `look` to inspect your surroundings for items and exits.
- Collect items to unlock new areas and build your roster.

### Example Walkthrough

Here’s how a game might unfold:

1. Start at Dodger Stadium.  
   _“You are at Dodger Stadium, home of the Los Angeles Dodgers.”_
2. Go east to Yankee Stadium:  
   `east`
3. Look around:  
   `look`  
   _“You see Clayton Kershaw, Manny Machado, Zac Gallen.”_
4. Take the Fenway Ticket:  
   `take FenwayTicket`
5. Go west, then north to Fenway Park:  
   `west and north`
6. Use the ticket to enter Fenway Park.
7. Collect three Red Sox players or encounter Shohei Ohtani for the Babe Ruth challenge!
   - Input "????-????" to win!

---

## High-Level Code Summary

### New Code Highlights

1. **Streamlined Movement Commands:**
   - Mapped commands (`north`, `south`, etc.) directly to actions.
   - Added support for chaining with `and`.
2. **Hidden Challenge Implementation:**
   - `triggerBabeRuthChallenge`: Handles Shohei Ohtani's encounter and Babe Ruth's showdown logic.
3. **Locked Door Mechanics:**
   - Modified `move` logic to check for keys or tickets and display appropriate messages.
4. **Winning Conditions:**
   - Updated `haveWonGame` to include both team-based and hidden-challenge wins.

---

Enjoy your journey to baseball glory!

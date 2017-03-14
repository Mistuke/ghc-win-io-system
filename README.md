# Haskell I/O Manager

Nursery for a GHC IO manager using Windows completion ports as a base.

When available RIO will be used as well to reduce overhead.

This IO Manager is intended to handle:
  - Locks
  - File I/O
  - Network I/O
  - Console I/O
  - Process and monitoring
  - Events

This is based on work from Joseph Adams, Mikhail Glushenkov and Tamar Christina.

.struct PowerGloveTelemetry
    XPos .byte          ; 1st byte: Signed X-Coordinate
    YPos .byte          ; 2nd byte: Signed Y-Coordinate
    ZPos .byte          ; 3rd byte: Signed Z-Coordinate
    Rotation .byte      ; 4th byte: Wrist Rotation Angle (around Z-axis)
    Flex .byte          ; 5th byte: Finger Flex Sensors
    Buttons .byte       ; 6th byte: Control Pad Buttons
    Unknown1 .byte      ; 7th byte: Unknown/unused (00h)
    Unknown2 .byte      ; 8th byte: Unknown/unused (00h)
    Error .byte         ; 9th byte: Error Flags
.endstruct

POWERGLOVEDATASIZE = .sizeof(PowerGloveTelemetry)
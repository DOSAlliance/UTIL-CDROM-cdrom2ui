�������  󪾁 �<t<:t$߈F��D�$�,A<s����>��<�u��� �	�!�L�!� �  �/������	�t��  �/��s$>���� �>�� � ��e�	�!��������/>��>:�r1�>���tNxL��:tEC��>�� ����	�!��1�>���<w��A ��!�	���!C�躸>��<t���C���	�tA��� �9�t<tG��� ��� �4�	�[�!�ڴ	�!���	�!�أ���s�Q����	�I�!� L�!�  �� �������� �� � ���1�>�����/r>��� �t�����ú�<t
��<t���	�!��           ��     �  
Usage: CDROM [EJECT|CLOSE|LOCK|UNLOCK|RESET] X:
Example: CDROM EJECT N:

Ask *CDEX 2.0+ if a CD-ROM exists: CDROM N:

Errorlevels: 0 okay, 1 failed / no such drive.
Not all drives do all commands.
$Unknown *CDEX error.
$CD-ROM drive not known to *CDEX.
$CD-ROM sys driver reports failure.
$Drive IOCTL not supported!?
$+Q1V<e7[Co  EJECT$CLOSE$LOCK$UNLOCK$RESET$Done.
$Failed.
$Command: $Using old *CDEX 1.x: CD-ROM range is A: - A: !?
$A: is no CD-ROM drive, but < $: $> is.$
$> are.$
$No *CDEX (SHSUCDX...) found, no CD-ROM access.
$�
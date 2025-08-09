import './Editor.css';
import { useState, useRef, useEffect, useCallback } from 'react';

export function Editor({ header, x, y, containerRef, onChange }) {
    const editorRef = useRef(null);
    const textareaRef = useRef(null);
    const [position, setPosition] = useState({ x, y });
    const [dragging, setDragging] = useState(false);
    const [dragOffset, setDragOffset] = useState({ x: 0, y: 0 });
    const [content, setContent] = useState('');

    const onMouseDown = useCallback((e) => {
        if (e.target.className.includes('drag-handle')) {
            setDragging(true);

            const { left, top } = editorRef.current.getBoundingClientRect();

            setDragOffset({
                x: e.clientX - left,
                y: e.clientY - top,
            });

            e.preventDefault();
        }
    }, []);

    const onMouseUp = useCallback(() => {
        setDragging(false);
    }, []);

    const onMouseMove = useCallback((e) => {
        if (!dragging) return;

        const containerRect = containerRef.current.getBoundingClientRect();
        const editorRect = editorRef.current.getBoundingClientRect();

        let newX = e.clientX - containerRect.left - dragOffset.x;
        let newY = e.clientY - containerRect.top - dragOffset.y;

        newX = Math.max(0, newX);
        newY = Math.max(0, newY);
        newX = Math.min(containerRect.width - editorRect.width, newX);
        newY = Math.min(containerRect.height - editorRect.height, newY);

        setPosition({ x: newX, y: newY });

    }, [dragging, dragOffset]);

    const adjustTextareaHeight = useCallback(() => {
        const textarea = textareaRef.current;
        if (textarea) {
            // Reset height to auto to get the correct scrollHeight
            textarea.style.height = 'auto';
            
            // Set height based on scrollHeight with a minimum height
            const minHeight = 60; // Minimum height for about 2 rows
            const newHeight = Math.max(minHeight, textarea.scrollHeight);
            
            textarea.style.height = `${newHeight}px`;
        }
    }, []);

    const handleKeyDown = useCallback((e) => {
        if (e.metaKey && e.key === 'Enter') {
            onChange(content);
        }
    }, [content, onChange]);

    const handleChange = useCallback((e) => {
        setContent(e.target.value);
    }, []);

    // Adjust height whenever content changes
    useEffect(() => {
        adjustTextareaHeight();
    }, [content, adjustTextareaHeight]);

    // Also adjust height on the next frame to ensure proper measurement
    useEffect(() => {
        const timeoutId = setTimeout(() => {
            adjustTextareaHeight();
        }, 0);
        
        return () => clearTimeout(timeoutId);
    }, [content, adjustTextareaHeight]);

    useEffect(() => {
        if (dragging) {
            document.addEventListener('mousemove', onMouseMove);
            document.addEventListener('mouseup', onMouseUp);
        }

        return () => {
            document.removeEventListener('mousemove', onMouseMove);
            document.removeEventListener('mouseup', onMouseUp);
        };
    }, [dragging, onMouseMove, onMouseUp]);

    return (
        <div
            className="overlay-editor"
            ref={editorRef}
            style={{
                top: `${position.y}px`,
                left: `${position.x}px`,
            }}
            onMouseDown={onMouseDown}
        >
            <div className="drag-handle">
                {header}
            </div>
            <textarea
                ref={textareaRef}
                value={content}
                onChange={handleChange}
                onKeyDown={handleKeyDown}
                rows={2}
            />
        </div>
    );
}